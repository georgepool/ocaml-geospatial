open Layer

let () = 
  let layer = UInt8BaseLayer.layer_from_file "test/cea.tiff" in
  UInt8BaseLayer.pp_layer layer;
  UInt8BaseLayer.set_window layer 0 0 5 5;
  let layer_total = UInt8BaseLayer.sum_layer layer in 
  Eio.traceln "Total: %i" layer_total; 

  Eio.traceln "----------------------------------";
  let empty_layer = UInt8BaseLayer.empty_layer_like layer in 
  let empty_layer_total = UInt8BaseLayer.sum_layer empty_layer in 
  
  Eio.traceln "Empty Total: %i" empty_layer_total; 
  Eio.traceln "----------------------------------";

  let layer1op = UInt8OperationLayer.operation_layer layer in
  let layer2op = UInt8OperationLayer.operation_layer empty_layer in 

  let operation = UInt8OperationLayer.add_layer layer1op layer2op in

  let output_layer = UInt8OperationLayer.eval_layer_operation operation in


  Eio.traceln "The total of the operation: %i" (UInt8BaseLayer.sum_layer output_layer); 
  Eio.traceln "----------------------------------";
  Eio.traceln "Now going to add 1 to everything in that output...";

  let layer3op = UInt8OperationLayer.operation_layer output_layer in
  let layer4op = UInt8OperationLayer.add layer3op 1 in 
  let output_layer2 = UInt8OperationLayer.eval_layer_operation layer4op in 
  Eio.traceln "The total after adding 1 to everything: %i" (UInt8BaseLayer.sum_layer output_layer2);

  Eio.traceln "----------------------------------";

  let layer5op = UInt8OperationLayer.add layer2op 1 in 
  let output_layer3 = UInt8OperationLayer.eval_layer_operation layer5op in 
  Eio.traceln "The total after adding 1 to empty layer: %i" (UInt8BaseLayer.sum_layer output_layer3);

  Eio.traceln "----------------------------------\n\n\n";

  let map_layer = UInt8BaseLayer.map_layer layer (fun x -> x + 1) in
  Eio.traceln "Map_layer_total: %i" (UInt8BaseLayer.sum_layer map_layer);

  let area = UInt8BaseLayer.underlying_area layer in 

  Area.pp_area area;

  let intersection_area = UInt8BaseLayer.find_intersection [layer; empty_layer] in

  let intersection_window = UInt8BaseLayer.set_window_from_area layer intersection_area in 

  Area.pp_area intersection_area;

  Window.pp_window intersection_window;

  print_endline "Running application with Layer library!";

