open Bigarray

let () = 
  let layer = Layer.layer_from_file "test/cea.tiff" in
  let width = Layer.width layer in 
  let height = Layer.height layer in 
  let area = Layer.area layer in
  let data = Layer.data layer in 
  Eio.traceln "File size: %ix%i" width height;
  let layer_total = Layer.sum_layer layer in 
  Eio.traceln "Total: %i" layer_total; 
  Eio.traceln "Length of array: %i" (Array1.dim data);

  let empty_layer = Layer.empty_layer_like layer in 
  let empty_layer_total = Layer.sum_layer empty_layer in 
  Eio.traceln "Empty Total: %i" empty_layer_total; 

  Layer.pp_area area;
  print_endline "Running application with Layer library!";