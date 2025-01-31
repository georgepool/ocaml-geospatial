open Layer

let () =
  let layer = FloatLayer.layer_from_file "test/jung_uncompressed.tif" in
  FloatLayer.pp_layer layer;;
