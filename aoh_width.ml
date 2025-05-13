open Layer
open Core_bench

let () =

  let species_names = [|
    "tile_01"; "tile_02"; 
    "tile_03"; "tile_04"; "tile_05"; "tile_06"; "tile_07"; "tile_08"; "tile_09";
    "tile_10"; "tile_11"; "tile_12"; "tile_13"; "tile_14"; "tile_15"; "tile_16"; "tile_17"; "tile_18"; "tile_19";
    "tile_20"; "tile_21"; "tile_22"; "tile_23"; "tile_24"; "tile_25"; "tile_26"; "tile_27"; "tile_28"; "tile_29";
    "tile_30"; "tile_31"; "tile_32"; "tile_33"; "tile_34"; "tile_35"; "tile_36"; "tile_37"; "tile_38"; "tile_39";
    "tile_40"; "tile_41"; "tile_42"; "tile_43"; "tile_44"; "tile_45";
    "tile_46"; "tile_47"; "tile_48"; "tile_49";
    "tile_50"; "tile_51"; "tile_52"; "tile_53"; "tile_54"; "tile_55"; "tile_56"; "tile_57"; "tile_58"; "tile_59";
    "tile_60"; "tile_61"; "tile_62"; "tile_63"; "tile_64"; "tile_65"; "tile_66"; "tile_67"; "tile_68"; "tile_69";
    "tile_70"; "tile_71"; "tile_72"; "tile_73"; "tile_74"; "tile_75"; "tile_76"; "tile_77"; "tile_78"; "tile_79";
    "tile_80"; "tile_81"; "tile_82"; "tile_83"; "tile_84"; "tile_85"; "tile_86"; "tile_87"; "tile_88"; "tile_89";
    "tile_90"; "tile_91"; "tile_92"; "tile_93"; "tile_94"
  |] in
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in 

  let jung_layer = FloatLayer.layer_from_file Eio.Path.(fs / "tests/files/jung_uncompressed.tif") in
  let elevation_layer = FloatLayer.layer_from_file Eio.Path.(fs / "tests/files/elevation-float_uncompressed.tif") in
  let area_per_pixel_layer = FloatLayer.layer_from_file Eio.Path.(fs / "tests/files/fullarea_uncompressed.tif") in

  let aoh species =
    Eio.traceln "Species code: %s" species;
    let species_file = species ^ ".tif" in 
    let raster_layer = FloatLayer.layer_from_file Eio.Path.(fs / "tests/files/variable_widths/" / species_file) in
    (* let raster_layer = FloatLayer.layer_from_file ("tests/files/9997_RESIDENT_uncompressed.tif") in *)
    let intersection_area = FloatLayer.find_intersection [jung_layer; elevation_layer; raster_layer; area_per_pixel_layer] in
    FloatLayer.update_layer_from_area jung_layer intersection_area;
    FloatLayer.update_layer_from_area elevation_layer intersection_area;
    FloatLayer.update_layer_from_area raster_layer intersection_area;
    FloatLayer.update_layer_from_area area_per_pixel_layer intersection_area;

    let elevation_lower, elevation_upper, jung_codes = 0.0, 3800.0, [200.0; 201.0; 600.0] in 
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
