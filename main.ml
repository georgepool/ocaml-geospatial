open Bigarray

let () = 
  let layer = Layer.layer_from_file "test/cea.tiff" in
  let width = Layer.width layer in 
  let height = Layer.height layer in 
  let area = Layer.underlying_area layer in
  let data = Layer.data layer in 
  Eio.traceln "File size: %ix%i" width height;
  let layer_total = Layer.sum_layer layer in 
  Eio.traceln "Total: %i" layer_total; 
  Eio.traceln "Length of array: %i" (Array1.dim data);

  let empty_layer = Layer.empty_layer_like layer in 
  let empty_layer_total = Layer.sum_layer empty_layer in 
  Eio.traceln "Empty Total: %i" empty_layer_total; 

  let map_layer = Layer.map_layer layer (fun x -> x + 1) in
  Eio.traceln "Map_layer_total: %i" (Layer.sum_layer map_layer);

  Layer.pp_area area;

  let intersection_area = Layer.find_intersection [layer; empty_layer] in

  let intersection_window = Layer.set_window_from_area layer intersection_area in 

  Layer.pp_area intersection_area;

  Layer.pp_window intersection_window;

  print_endline "Running application with Layer library!";