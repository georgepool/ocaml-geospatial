(* open Bigarray *)
open Tiff
open Layer

let () = 
  let layer = BaseLayer.layer_from_file "test/cea.tiff" Data.UINT8 in
  BaseLayer.pp_layer layer;
  BaseLayer.set_window layer 0 0 5 5;
  let layer_total = BaseLayer.sum_layer layer in 
  Eio.traceln "Total: %i" layer_total; 

  Eio.traceln "----------------------------------";
  let empty_layer = BaseLayer.empty_layer_like layer in 
  let empty_layer_total = BaseLayer.sum_layer empty_layer in 
  
  Eio.traceln "Empty Total: %i" empty_layer_total; 
  Eio.traceln "----------------------------------";

  let layer1op = OperationLayer.operation_layer layer in
  let layer2op = OperationLayer.operation_layer empty_layer in 

  let operation = OperationLayer.add_layer layer1op layer2op in

  let output_layer = OperationLayer.eval_layer_operation operation in


  Eio.traceln "The total of the operation: %i" (BaseLayer.sum_layer output_layer); 
  Eio.traceln "----------------------------------";
  Eio.traceln "Now going to add 1 to everything in that output...";

  let layer3op = OperationLayer.operation_layer output_layer in
  let layer4op = OperationLayer.add layer3op 1 in 
  let output_layer2 = OperationLayer.eval_layer_operation layer4op in 
  Eio.traceln "The total after adding 1 to everything: %i" (BaseLayer.sum_layer output_layer2);

  Eio.traceln "----------------------------------";

  let layer5op = OperationLayer.add layer2op 1 in 
  let output_layer3 = OperationLayer.eval_layer_operation layer5op in 
  Eio.traceln "The total after adding 1 to empty layer: %i" (BaseLayer.sum_layer output_layer3);

  Eio.traceln "----------------------------------\n\n\n";

  (* let map_layer = BaseLayer.map_layer layer (fun x -> x + 1) in
  Eio.traceln "Map_layer_total: %i" (Layer.sum_layer map_layer);

  Layer.pp_area area;

  let intersection_area = Layer.find_intersection [layer; empty_layer] in

  let intersection_window = Layer.set_window_from_area layer intersection_area in 

  Layer.pp_area intersection_area;

  Layer.pp_window intersection_window;

  print_endline "Running application with Layer library!";
 *)
