open Layer
open Core_bench
open Yojson.Basic.Util

let load_crosswalk filepath =
  let table = Hashtbl.create 100 in
  let csv = Csv.load filepath in
  let rows = 
    match csv with 
    | _header :: data -> data
    | [] -> []
  in
  List.iter (fun row ->
    match row with
    | [code; value_str] ->
        let value = float_of_string value_str in
        let existing_values =
          match Hashtbl.find_opt table code with
          | Some lst -> lst
          | None -> []
        in
        Hashtbl.replace table code (value :: existing_values)
    | _ -> () (* skip bad rows *)
  ) rows;
  table

let get_jung_values jung_string crosswalk_table =
  let codes = String.split_on_char '|' jung_string in
  let rec collect_values acc = function
    | [] -> acc
    | code :: rest ->
        let values =
          match Hashtbl.find_opt crosswalk_table code with
          | Some vals -> vals
          | None ->
              Eio.traceln "Warning: Code %s not found in crosswalk" code;
              []
        in
        collect_values (values @ acc) rest
  in
  collect_values [] codes

let parse_geojson filepath crosswalk_table = 
  let geo_json = Yojson.Basic.from_file filepath in 
  let features = geo_json |> member "features" |> to_list in 
  let first_feature = List.hd features in
  let props = first_feature |> member "properties" in
  let elevation_lower =
    props
    |> member "elevation_lower"
    |> to_string
    |> float_of_string
  in
  
  let elevation_upper =
    props
    |> member "elevation_upper"
    |> to_string
    |> float_of_string
  in

  let jung_string = 
    props
    |> member "full_habitat_code"
    |> to_string
  in

  let jung_codes = get_jung_values jung_string crosswalk_table in 

  elevation_lower, elevation_upper, jung_codes


let () =
  let crosswalk_table = load_crosswalk "tests/files/crosswalk.csv" in
  (* some of these don't work with current OCaml-TIFF*)
  (* let species_names = [| "10355_RESIDENT"; "10356_RESIDENT"; "101754194_RESIDENT";  "103656912_BREEDING"; "103656907_NONBREEDING"; "100339944_RESIDENT"; "101949823_RESIDENT"; "10109_RESIDENT"; "103656367_RESIDENT"; "10644_RESIDENT"; "101949735_RESIDENT"; "103656903_BREEDING"; "101742312_RESIDENT"; "101742396_RESIDENT"; "107283813_RESIDENT"; "101949760_RESIDENT"; "103656357_RESIDENT"; "103656929_RESIDENT"; "10617_RESIDENT"; "10151_RESIDENT"; "103656476_RESIDENT"; "103656460_RESIDENT"; "10005_RESIDENT"; "10152_RESIDENT"; "101754561_RESIDENT"; "10634_RESIDENT"; "101742205_RESIDENT"; "101754523_RESIDENT"; "10211_RESIDENT"; "10170_RESIDENT"; "101754336_RESIDENT"; "101754534_RESIDENT"; "100099193_RESIDENT"; "10350_RESIDENT"; "10054_RESIDENT"; "10616_RESIDENT"; "100339930_RESIDENT"; "103656444_RESIDENT"; "107580991_RESIDENT"; "103656924_RESIDENT"; "102208365_RESIDENT"; "103656944_RESIDENT"; "101741429_RESIDENT"; "107282713_RESIDENT"; "101228458_RESIDENT"; "103656387_RESIDENT"; "102998975_RESIDENT"; "10130_RESIDENT"; "10123_RESIDENT"; "10112_RESIDENT"; "101949805_RESIDENT"; "101741031_RESIDENT"; "101742350_RESIDENT"; "101752419_RESIDENT"; "101741020_RESIDENT"; "103656402_RESIDENT"; "103656920_RESIDENT"; "105209514_RESIDENT"; "103656341_RESIDENT"; "103656470_RESIDENT"; "10126_RESIDENT"; "10108_RESIDENT"; "10124_RESIDENT"; "56003281_RESIDENT"; "101752494_RESIDENT"; "10053_RESIDENT"; "103656907_BREEDING"; "103656912_NONBREEDING"; "101949746_RESIDENT"; "101754514_RESIDENT"; "10215_RESIDENT"; "10615_RESIDENT"; "10618_RESIDENT"; "10203_RESIDENT"; "10134_RESIDENT"; "103656903_NONBREEDING"; "10357_RESIDENT"; "10117_RESIDENT"; "105209254_RESIDENT"; "103656371_RESIDENT"; "10041_RESIDENT"; "10613_RESIDENT"; "101741721_RESIDENT"; "101754410_RESIDENT"; "103656434_RESIDENT"; "103656466_RESIDENT"; "107282592_RESIDENT"; "9997_RESIDENT"; "10150_RESIDENT"; "103656933_RESIDENT"; "102998728_RESIDENT"; "101949793_RESIDENT"; "10007_RESIDENT"; "10351_RESIDENT"; "10010_RESIDENT"; "101949771_RESIDENT"; "10566_RESIDENT"; "10167_RESIDENT"; "10009_RESIDENT"; "13425_RESIDENT"; "10459_RESIDENT"; "107283970_RESIDENT"; "10200_RESIDENT" |] in *)
  
  (* these all work with current OCaml-TIFF*)
  let species_names = [| "10355_RESIDENT"; "10356_RESIDENT"; "101754194_RESIDENT"; "103656907_NONBREEDING"; "100339944_RESIDENT"; "101949823_RESIDENT"; "10109_RESIDENT"; "103656367_RESIDENT"; "10644_RESIDENT"; "101949735_RESIDENT"; "103656903_BREEDING"; "101742312_RESIDENT"; "101742396_RESIDENT"; "107283813_RESIDENT"; "103656357_RESIDENT"; "103656929_RESIDENT"; "10151_RESIDENT"; "103656476_RESIDENT"; "103656460_RESIDENT"; "10005_RESIDENT"; "10152_RESIDENT"; "101754561_RESIDENT"; "10634_RESIDENT"; "101742205_RESIDENT"; "101754523_RESIDENT"; "10211_RESIDENT"; "10170_RESIDENT"; "101754336_RESIDENT"; "101754534_RESIDENT"; "100099193_RESIDENT"; "10350_RESIDENT"; "10054_RESIDENT"; "10616_RESIDENT"; "103656444_RESIDENT"; "107580991_RESIDENT"; "103656924_RESIDENT"; "102208365_RESIDENT"; "103656944_RESIDENT"; "101741429_RESIDENT"; "107282713_RESIDENT"; "101228458_RESIDENT"; "103656387_RESIDENT"; "102998975_RESIDENT"; "10130_RESIDENT"; "10123_RESIDENT"; "10112_RESIDENT"; "101949805_RESIDENT"; "101741031_RESIDENT"; "101742350_RESIDENT"; "101752419_RESIDENT"; "101741020_RESIDENT"; "103656402_RESIDENT"; "103656920_RESIDENT"; "105209514_RESIDENT"; "103656341_RESIDENT"; "103656470_RESIDENT"; "10126_RESIDENT"; "10108_RESIDENT"; "10124_RESIDENT"; "56003281_RESIDENT"; "101752494_RESIDENT"; "10053_RESIDENT"; "103656907_BREEDING"; "101949746_RESIDENT"; "101754514_RESIDENT"; "10215_RESIDENT"; "10203_RESIDENT"; "10134_RESIDENT"; "103656903_NONBREEDING"; "10357_RESIDENT"; "10117_RESIDENT"; "105209254_RESIDENT"; "103656371_RESIDENT"; "10041_RESIDENT"; "10613_RESIDENT"; "103656434_RESIDENT"; "103656466_RESIDENT"; "107282592_RESIDENT"; "9997_RESIDENT"; "10150_RESIDENT"; "103656933_RESIDENT"; "102998728_RESIDENT"; "101949793_RESIDENT"; "10007_RESIDENT"; "10351_RESIDENT"; "10010_RESIDENT"; "101949771_RESIDENT"; "10566_RESIDENT"; "10167_RESIDENT"; "10009_RESIDENT"; "13425_RESIDENT"; "10459_RESIDENT"; "107283970_RESIDENT"; "10200_RESIDENT" |] in
  (* let species_names = [|"9997_RESIDENT"|] in *)
  let jung_layer = FloatLayer.layer_from_file "tests/files/jung_uncompressed.tif" in
  let elevation_layer = FloatLayer.layer_from_file "tests/files/elevation-float_uncompressed.tif" in
  let area_per_pixel_layer = FloatLayer.layer_from_file "tests/files/fullarea_uncompressed.tif" in

  let aoh species =
    Eio.traceln "Species code: %s" species;
    let raster_layer = FloatLayer.layer_from_file ("tests/files/species/" ^ species ^ ".tif") in
    (* let raster_layer = FloatLayer.layer_from_file ("tests/files/9997_RESIDENT_uncompressed.tif") in *)
    let intersection_area = FloatLayer.find_intersection [jung_layer; elevation_layer; raster_layer; area_per_pixel_layer] in
    FloatLayer.update_layer_from_area jung_layer intersection_area;
    FloatLayer.update_layer_from_area elevation_layer intersection_area;
    FloatLayer.update_layer_from_area raster_layer intersection_area;
    FloatLayer.update_layer_from_area area_per_pixel_layer intersection_area;

    let elevation_lower, elevation_upper, jung_codes = parse_geojson ("tests/files/species/" ^ species ^ ".geojson") crosswalk_table in 
    let jung = FloatOperationLayer.operation_layer jung_layer in 
    let elevation = FloatOperationLayer.operation_layer elevation_layer in 
    let raster = FloatOperationLayer.operation_layer raster_layer in 
    let area_per_pixel = FloatOperationLayer.operation_layer area_per_pixel_layer in

    let elevation_filtered = FloatOperationLayer.layer_values_in_range elevation elevation_lower elevation_upper in
    let jung_filtered = FloatOperationLayer.layer_values_in_list jung jung_codes in

    let pixels = FloatOperationLayer.mul_layer (FloatOperationLayer.mul_layer elevation_filtered jung_filtered) raster in

    let final_aoh = FloatOperationLayer.mul_layer pixels area_per_pixel in 

    let pixels_sum = FloatOperationLayer.sum_layer pixels in
    let final_area_sum = FloatOperationLayer.sum_layer final_aoh in 
    Eio.traceln "Pixels sum: %f" pixels_sum;
    Eio.traceln "Final area: %f" final_area_sum; 
    Eio.traceln "---------------------"; 

    FloatLayer.reset_layer jung_layer;
    FloatLayer.reset_layer elevation_layer;
    FloatLayer.reset_layer area_per_pixel_layer;
  in

  let bench_tests = ref [] in

  for i = 0 to (Array.length species_names) - 1 do

    let species = species_names.(i) in 
    (* Eio.traceln "species: %s" species; *)
    bench_tests := !bench_tests @ [Bench.Test.create ~name:species (fun () -> aoh species)];
  done;

  (* Eio.traceln "length: %i" (List.length !bench_tests); *)

  Bench.make_command !bench_tests |> Command_unix.run
