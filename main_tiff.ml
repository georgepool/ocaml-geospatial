open Eio

let () =
  Eio_main.run @@ fun env ->
  let fs = Stdenv.fs env in
  Path.(with_open_in (fs / "test/cea.tiff")) @@ fun r ->
  let tiff = Tiff.from_file (File.pread_exact r) in
  let ifd = Tiff.ifd tiff in
  let entries = Tiff.Ifd.entries ifd in
  let height = Tiff.Ifd.height ifd in
  let width = Tiff.Ifd.width ifd in
  Eio.traceln "Entries:\n %a" Fmt.(list Tiff.Ifd.pp_entry) entries;
  Eio.traceln "File size: %ix%i" height width;
  Eio.traceln "Samples per pixel: %i" (Tiff.Ifd.samples_per_pixel ifd);
  Eio.traceln "Bits per sample: %a"
    Fmt.(list int)
    (Tiff.Ifd.bits_per_sample ifd);
  let data_offsets = Tiff.Ifd.data_offsets ifd in
  let data_bytecounts = Tiff.Ifd.data_bytecounts ifd in
  Eio.traceln "Offsets: %a" Fmt.(list ~sep:(any ", ") int) data_offsets;
  Eio.traceln "Counts: %a" Fmt.(list ~sep:(any ", ") int) data_bytecounts;

  let geokey_entries = Tiff.Ifd.GeoKeys.entries ifd in
  Eio.traceln "GeoKey Entries:\n %a"
    Fmt.(list Tiff.Ifd.GeoKeys.pp_entry)
    (Tiff.Ifd.GeoKeys.get_geo_entries geokey_entries);
  let model_tiepoint = Tiff.Ifd.tiepoint ifd in
  let model_pixel_scale = Tiff.Ifd.pixel_scale ifd in
  let print_float_array fmt arr =
    Format.fprintf fmt "[";
    Array.iteri
      (fun i x ->
        if i > 0 then Format.fprintf fmt "; ";
        Format.fprintf fmt "%f" x)
      arr;
    Format.fprintf fmt "]"
  in
  Eio.traceln "Model tiepoints: %a" print_float_array model_tiepoint;
  Eio.traceln "Model pixel scales: %a" print_float_array model_pixel_scale;

  let data = Tiff.data tiff (File.pread_exact r) Tiff.Data.UINT8 in

  let sum =
    match data with
    | Tiff.Data.UInt8Data arr -> Owl.Dense.Ndarray.Generic.sum' arr
    | _ -> raise (Invalid_argument "Tiff Data has wrong type")
  in
  Eio.traceln "New total: %i" sum;
  Eio.traceln "File opened successfully."
