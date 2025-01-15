open Bigarray
open Tiff

let () = 
  let layer = Layer.layer_from_file "test/cea.tiff" Data.UINT8 in
  let width = Layer.width layer in 
  let height = Layer.height layer in 
  let area = Layer.underlying_area layer in
  let data = Layer.data layer in 
  Eio.traceln "File size: %ix%i" width height;
  Layer.set_window layer 0 0 5 5;
  let layer_total = Layer.sum_layer layer in 
  Eio.traceln "Total: %i" layer_total; 
  Eio.traceln "Length of array: %i" (Array1.dim data);


  Eio.traceln "----------------------------------";
  let empty_layer = Layer.empty_layer_like layer in 
  let empty_layer_total = Layer.sum_layer empty_layer in 

  
  Eio.traceln "Empty Total: %i" empty_layer_total; 
  Eio.traceln "----------------------------------";

  let operation = Layer.add_layer layer empty_layer in

  let output_layer = Layer.eval_layer operation in


  Eio.traceln "The total of the operation: %i" (Layer.sum_layer output_layer); 
  Eio.traceln "----------------------------------";
  Eio.traceln "Now going to add 1 to everything in that output...";

  let operation2 = Layer.add operation 1 in 
  let output_layer2 = Layer.eval_layer operation2 in 
  Eio.traceln "The total after adding 1 to everything: %i" (Layer.sum_layer output_layer2);

  Eio.traceln "----------------------------------";

  let operation3 = Layer.add empty_layer 1 in 
  let output_layer3 = Layer.eval_layer operation3 in 
  Eio.traceln "The total after adding 1 to empty layer: %i" (Layer.sum_layer output_layer3);

  Eio.traceln "----------------------------------\n\n\n";

  let map_layer = Layer.map_layer layer (fun x -> x + 1) in
  Eio.traceln "Map_layer_total: %i" (Layer.sum_layer map_layer);

  Layer.pp_area area;

  let intersection_area = Layer.find_intersection [layer; empty_layer] in

  let intersection_window = Layer.set_window_from_area layer intersection_area in 

  Layer.pp_area intersection_area;

  Layer.pp_window intersection_window;

  print_endline "Running application with Layer library!";

