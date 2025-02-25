open Layer

let () =
  let jung_layer = FloatLayer.layer_from_file "test/jung_uncompressed.tif" in
  let elevation_layer = FloatLayer.layer_from_file "test/elevation-float_uncompressed.tif" in
  let raster_layer = FloatLayer.layer_from_file "test/9997_RESIDENT_uncompressed.tif" in
  let area_per_pixel_layer = FloatLayer.layer_from_file "test/fullarea_uncompressed.tif" in
  let intersection_area = FloatLayer.find_intersection [jung_layer; elevation_layer; raster_layer; area_per_pixel_layer] in 
  FloatLayer.update_layer_from_area jung_layer intersection_area;
  FloatLayer.update_layer_from_area elevation_layer intersection_area;
  FloatLayer.update_layer_from_area raster_layer intersection_area;
  FloatLayer.update_layer_from_area area_per_pixel_layer intersection_area;
  let jung = FloatOperationLayer.operation_layer jung_layer in
  let elevation = FloatOperationLayer.operation_layer elevation_layer in
  let raster = FloatOperationLayer.operation_layer raster_layer in
  let area_per_pixel = FloatOperationLayer.operation_layer area_per_pixel_layer in 
  let elevation_filtered = FloatOperationLayer.layer_values_in_range elevation 0.0 3800.0 in
  let jung_filtered = FloatOperationLayer.layer_values_in_list jung [200.0; 201.0; 600.0] in
  let pixels = FloatOperationLayer.mul_layer (FloatOperationLayer.mul_layer elevation_filtered jung_filtered) raster in
  let final_aoh = FloatOperationLayer.mul_layer pixels area_per_pixel in 
  let pixels_sum = FloatOperationLayer.sum_layer pixels in
  let final_area_sum = FloatOperationLayer.sum_layer final_aoh in 
  Eio.traceln "Pixels sum: %f" pixels_sum;
  Eio.traceln "Final area: %f" final_area_sum;
