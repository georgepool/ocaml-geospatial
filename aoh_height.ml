open Layer
open Core_bench

let () =
  (* some of these don't work with current OCaml-TIFF*)
  (* let species_names = [| "10355_RESIDENT"; "10356_RESIDENT"; "101754194_RESIDENT";  "103656912_BREEDING"; "103656907_NONBREEDING"; "100339944_RESIDENT"; "101949823_RESIDENT"; "10109_RESIDENT"; "103656367_RESIDENT"; "10644_RESIDENT"; "101949735_RESIDENT"; "103656903_BREEDING"; "101742312_RESIDENT"; "101742396_RESIDENT"; "107283813_RESIDENT"; "101949760_RESIDENT"; "103656357_RESIDENT"; "103656929_RESIDENT"; "10617_RESIDENT"; "10151_RESIDENT"; "103656476_RESIDENT"; "103656460_RESIDENT"; "10005_RESIDENT"; "10152_RESIDENT"; "101754561_RESIDENT"; "10634_RESIDENT"; "101742205_RESIDENT"; "101754523_RESIDENT"; "10211_RESIDENT"; "10170_RESIDENT"; "101754336_RESIDENT"; "101754534_RESIDENT"; "100099193_RESIDENT"; "10350_RESIDENT"; "10054_RESIDENT"; "10616_RESIDENT"; "100339930_RESIDENT"; "103656444_RESIDENT"; "107580991_RESIDENT"; "103656924_RESIDENT"; "102208365_RESIDENT"; "103656944_RESIDENT"; "101741429_RESIDENT"; "107282713_RESIDENT"; "101228458_RESIDENT"; "103656387_RESIDENT"; "102998975_RESIDENT"; "10130_RESIDENT"; "10123_RESIDENT"; "10112_RESIDENT"; "101949805_RESIDENT"; "101741031_RESIDENT"; "101742350_RESIDENT"; "101752419_RESIDENT"; "101741020_RESIDENT"; "103656402_RESIDENT"; "103656920_RESIDENT"; "105209514_RESIDENT"; "103656341_RESIDENT"; "103656470_RESIDENT"; "10126_RESIDENT"; "10108_RESIDENT"; "10124_RESIDENT"; "56003281_RESIDENT"; "101752494_RESIDENT"; "10053_RESIDENT"; "103656907_BREEDING"; "103656912_NONBREEDING"; "101949746_RESIDENT"; "101754514_RESIDENT"; "10215_RESIDENT"; "10615_RESIDENT"; "10618_RESIDENT"; "10203_RESIDENT"; "10134_RESIDENT"; "103656903_NONBREEDING"; "10357_RESIDENT"; "10117_RESIDENT"; "105209254_RESIDENT"; "103656371_RESIDENT"; "10041_RESIDENT"; "10613_RESIDENT"; "101741721_RESIDENT"; "101754410_RESIDENT"; "103656434_RESIDENT"; "103656466_RESIDENT"; "107282592_RESIDENT"; "9997_RESIDENT"; "10150_RESIDENT"; "103656933_RESIDENT"; "102998728_RESIDENT"; "101949793_RESIDENT"; "10007_RESIDENT"; "10351_RESIDENT"; "10010_RESIDENT"; "101949771_RESIDENT"; "10566_RESIDENT"; "10167_RESIDENT"; "10009_RESIDENT"; "13425_RESIDENT"; "10459_RESIDENT"; "107283970_RESIDENT"; "10200_RESIDENT" |] in *)
  
  (* these all work with current OCaml-TIFF*)
  let species_names = [|
    (* "tile_01"; "tile_02"; "tile_03"; "tile_04"; "tile_05"; "tile_06"; "tile_07"; "tile_08"; "tile_09";
    "tile_10"; "tile_11"; "tile_12"; "tile_13"; "tile_14"; "tile_15"; "tile_16"; "tile_17"; "tile_18"; "tile_19";
    "tile_20"; "tile_21"; "tile_22"; "tile_23"; "tile_24"; "tile_25"; "tile_26"; "tile_27"; "tile_28"; "tile_29";
    "tile_30"; "tile_31"; "tile_32"; "tile_33"; "tile_34"; "tile_35"; "tile_36"; "tile_37"; "tile_38"; "tile_39";
    "tile_40"; "tile_41"; "tile_42"; "tile_43"; "tile_44"; "tile_45"; "tile_46"; "tile_47"; "tile_48"; "tile_49";
    "tile_50"; "tile_51"; "tile_52"; "tile_53"; "tile_54"; "tile_55"; "tile_56"; "tile_57"; "tile_58"; "tile_59";
    "tile_60"; "tile_61"; "tile_62"; "tile_63"; "tile_64"; "tile_65"; "tile_66"; "tile_67"; "tile_68"; "tile_69";
    "tile_70"; "tile_71"; "tile_72"; "tile_73"; "tile_74"; "tile_75"; "tile_76"; "tile_77"; "tile_78"; "tile_79";
    "tile_80"; "tile_81"; "tile_82"; "tile_83"; "tile_84"; "tile_85"; *)
     "tile_86";
      (* "tile_87"; "tile_88"; "tile_89";
    "tile_90"; "tile_91"; "tile_92"; *)
     "tile_93"; "tile_94"
  |] in  (* let species_names = [|"9997_RESIDENT"|] in *)
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in 

  let jung_layer = FloatLayer.layer_from_file Eio.Path.(fs / "tests/files/jung_uncompressed.tif") in
  let elevation_layer = FloatLayer.layer_from_file Eio.Path.(fs / "tests/files/elevation-float_uncompressed.tif") in
  let area_per_pixel_layer = FloatLayer.layer_from_file Eio.Path.(fs / "tests/files/fullarea_uncompressed.tif") in

  let aoh species =
    Eio.traceln "Species code: %s" species;
    let species_file = species ^ ".tif" in 
    let raster_layer = FloatLayer.layer_from_file Eio.Path.(fs / "tests/files/variable_heights/" / species_file) in
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
