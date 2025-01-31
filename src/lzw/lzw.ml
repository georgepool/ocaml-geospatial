(*
Authors: Jan Dudek, Michael Dales
*)

let rec get_bits bytedata offset count =
  match count with
  | 0 -> 0
  | n ->
      let byte = int_of_char (Bytes.get bytedata (offset / 8)) in
      let bitoffset = offset mod 8 in
      let bit = (byte lsr bitoffset) land 1 in
      bit + (get_bits bytedata (offset + 1) (n - 1) lsl 1)

let flatten_codes ?pad bits_per_pixel code_list =
  let pad = match pad with None -> false | Some x -> x in
  let code_list =
    match pad with
    | false -> code_list
    | true ->
        let total_bits =
          List.fold_left (fun acc (_, bit_count) -> acc + bit_count) 0 code_list
        in
        let extra = bits_per_pixel - (total_bits mod bits_per_pixel) in
        List.rev ((Z.zero, extra) :: List.rev code_list)
  in
  let total_bits =
    List.fold_left (fun acc (_, bit_count) -> acc + bit_count) 0 code_list
  in
  if total_bits mod bits_per_pixel != 0 then
    failwith
      (Printf.sprintf "unaligned result: %d bits, %d bits per pixel" total_bits
         bits_per_pixel);
  let result = Bytes.create (total_bits / bits_per_pixel) in
  let _, (_, rem) =
    List.fold_left
      (fun (bit_offset, (rem_bits, rem_len)) (data, bit_count) ->
        let merged = (Z.((data lsl rem_len) + rem_bits), bit_count + rem_len) in

        let rec inner result_bit_offset (mbits, mlen) =
          let byte_offset = result_bit_offset / bits_per_pixel in

          if mlen < bits_per_pixel then (result_bit_offset, (mbits, mlen))
          else (
            Bytes.set result byte_offset
              (char_of_int
                 (Z.to_int (Z.logand mbits Z.((one lsl bits_per_pixel) - one))));
            inner
              (result_bit_offset + bits_per_pixel)
              (Z.shift_right mbits bits_per_pixel, mlen - bits_per_pixel))
        in
        inner bit_offset merged)
      (0, (Z.zero, 0))
      code_list
  in
  if rem != 0 then failwith (Printf.sprintf "%d bits remaining at end" rem);
  result

(* --- dekompresja ------------------------------------------------ *)

let add_codes a b =
  let a_data, a_bitcount = a and b_data, b_bitcount = b in
  (Z.(a_data lor (b_data lsl a_bitcount)), a_bitcount + b_bitcount)

let build_table_entry a b character_size =
  add_codes a (Z.(fst b land ((one lsl character_size) - one)), character_size)

let decode input initial_code_size =
  let clear_code = 1 lsl initial_code_size in
  let end_code = clear_code + 1 in
  let dict = Array.make 4096 (Z.zero, 0) in

  let rec inner in_offset code_size next_code_index prev_code =
    let code = get_bits input in_offset code_size in
    (* Printf.printf "get %d %d -> %x\n" in_offset code_size code;*)
    if code == clear_code then
      let new_code_size = initial_code_size + 1 in
      let next_code = get_bits input (in_offset + code_size) new_code_size in
      (Z.of_int next_code, initial_code_size)
      :: inner
           (in_offset + code_size + new_code_size)
           new_code_size (clear_code + 2)
           (Z.of_int next_code, initial_code_size)
    else if code == end_code then []
    else
      let entry =
        if code < clear_code then (Z.of_int code, initial_code_size)
        else if code < next_code_index then dict.(code)
        else build_table_entry prev_code prev_code initial_code_size
      in

      if next_code_index < 4096 then
        dict.(next_code_index) <-
          build_table_entry prev_code entry initial_code_size;
      let prev_entry, new_code_size, new_code_index =
        match next_code_index with
        | 4095 -> (prev_code, code_size, next_code_index)
        | _ ->
            let i = next_code_index + 1 in
            ( entry,
              (if i >= 1 lsl code_size then code_size + 1 else code_size),
              i )
      in

      entry
      :: inner (in_offset + code_size) new_code_size new_code_index prev_entry
  in

  let c = inner 0 (initial_code_size + 1) (clear_code + 2) (Z.zero, 0) in

  flatten_codes initial_code_size c

(* --- kompresja -------------------------------------------------- *)

(* Kompresja operuje na symbolach (symbolami sa numery kolorow z
   palety). Symbole sa skladane w ciagi, reprezentowane za pomoca
   list. Ciagom symboli przypisywane sa kody. Do mapowania list
   symboli na ich kody korzystamy ze slownika opartego o modul Map. *)

module EncDictOrderedType : Map.OrderedType with type t = int list = struct
  type t = int list

  let compare = compare
end

module EncDict = struct
  include Map.Make (EncDictOrderedType)

  (* Jesli ciag symboli ma tylko jeden element, to kodem takiego ciagu
     jest po prostu wartosc tego symbolu. Takich ciagow nie
     przechowujemy w slowniku. *)
  let find_word word dict = match word with [ c ] -> c | _ -> find word dict
end

(* Kompresja: z listy symboli (bajtow) funkcja tworzy liste
   kodow. Wynikowa list ma postac par: (kod, ilosc bitow), gdzie kod
   to int < 4096, ilosc bitow <= 12. *)
let make_codes input initial_code_size =
  let clear_code = 1 lsl initial_code_size in
  let end_code = clear_code + 1 in
  let rec encode input_index dict word code_size avail_code =
    if input_index / 8 < Bytes.length input then
      let char = get_bits input input_index initial_code_size in
      if word = [] then
        (clear_code, code_size)
        :: encode
             (input_index + initial_code_size)
             dict [ char ] (initial_code_size + 1) (clear_code + 2)
      else
        let word_char = char :: word in
        if EncDict.mem word_char dict then
          encode
            (input_index + initial_code_size)
            dict word_char code_size avail_code
        else
          let code = EncDict.find_word word dict in
          let new_avail_code = avail_code + 1 in
          let new_code_size =
            if new_avail_code > 1 lsl code_size then code_size + 1
            else code_size
          in
          let new_dict = EncDict.add word_char avail_code dict in
          (* jesli wykorzystano juz wszystkie 12-bitowe kody,
             zwracamy clear_code, czyscimy slownik i resetujemy
             dlugosc kodu do dlugosci poczatkowej *)
          if new_avail_code >= 0xFFF then
            (code, code_size) :: (clear_code, code_size)
            :: encode (input_index + 1) EncDict.empty [ char ]
                 (initial_code_size + initial_code_size)
                 (clear_code + 2)
          else
            (code, code_size)
            :: encode
                 (input_index + initial_code_size)
                 new_dict [ char ] new_code_size new_avail_code
    else
      (* kiedy skonczy sie wejscie, zwracamy kod dla symboli
         bedacych w buforze (o ile sa takie) oraz kod oznaczajacy
         koniec *)
      let ending = [ (end_code, code_size) ] in
      if word != [] then (EncDict.find_word word dict, code_size) :: ending
      else ending
  in
  let codes =
    encode 0 EncDict.empty [] (initial_code_size + 1) (clear_code + 2)
  in
  codes

let encode pixels_list code_size =
  make_codes pixels_list code_size
  |> List.map (fun (c, s) -> (Z.of_int c, s))
  |> flatten_codes ~pad:true 8
