open Layer

let () =
  let layer1 = FloatLayer.layer_from_file "tests/files/jung_uncompressed.tif" in
  FloatLayer.pp_layer layer1;

  (* Eio.traceln "Jung underlying_area:";
     Area.pp_area (FloatLayer.underlying_area layer1); *)
  let layer2 =
    FloatLayer.layer_from_file "tests/files/elevation-float_uncompressed.tif"
  in
  FloatLayer.pp_layer layer2;

  (* Eio.traceln "Elevation underlying_area:";
     Area.pp_area (FloatLayer.underlying_area layer2); *)

  (* let intersection_area = FloatLayer.find_intersection [ layer1; layer2] in

     FloatLayer.update_layer_from_area layer1 intersection_area;
     FloatLayer.update_layer_from_area layer2 intersection_area;

     Eio.traceln "---------------\nIntersection area: ";
     Area.pp_area intersection_area; *)
  FloatLayer.set_window layer1 0 0 1000 1000;

  FloatLayer.set_window layer2 0 0 1000 1000;

  FloatLayer.pp_layer layer1;
  FloatLayer.pp_layer layer2;

  let layerop1 = FloatOperationLayer.operation_layer layer1 in
  let layerop2 = FloatOperationLayer.operation_layer layer2 in
  let new_layerop = FloatOperationLayer.add_layer layerop1 layerop2 in
  let sum = FloatOperationLayer.sum_layer new_layerop in

  let sum1 = FloatOperationLayer.sum_layer layerop1 in
  let sum2 = FloatOperationLayer.sum_layer layerop2 in
  let sum3 = sum1 +. sum2 in
  Eio.traceln "Total sum: %f" sum;
  Eio.traceln "Sum should be: %f" sum3
