open Layer

let () =
  let layer1 = FloatLayer.layer_from_file "test/jung_uncompressed.tif" in
  FloatLayer.pp_layer layer1;
  (* Eio.traceln "Jung underlying_area:";
  Area.pp_area (FloatLayer.underlying_area layer1); *)

  let layer2 = FloatLayer.layer_from_file "test/elevation-float_uncompressed.tif" in 
  FloatLayer.pp_layer layer2;
  (* Eio.traceln "Elevation underlying_area:";
  Area.pp_area (FloatLayer.underlying_area layer2); *)

  let intersection_area = FloatLayer.find_intersection [ layer1; layer2] in 

  FloatLayer.update_layer_from_area layer1 intersection_area;
  FloatLayer.update_layer_from_area layer2 intersection_area;

  Eio.traceln "---------------\nIntersection area: ";
  Area.pp_area intersection_area;

  FloatLayer.pp_layer layer1;
  FloatLayer.pp_layer layer2;

  let layerop1 = FloatOperationLayer.operation_layer layer1 in 
  let layerop2 = FloatOperationLayer.operation_layer layer2 in 
  let new_layer = FloatOperationLayer.add_layer layerop1 layerop2 in 
  let result_layer = FloatOperationLayer.eval_layer_operation new_layer in
  FloatLayer.pp_layer result_layer;
  
