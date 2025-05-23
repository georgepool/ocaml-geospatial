open Eio
open Bigarray

module BaseLayer = struct
  exception DataNotReadYet

  type tiff_info = { file_name : Eio.Fs.dir_ty Eio.Path.t; tiff : Tiff.t }

  type ('a, 'b) t = {
    width : int;
    height : int;
    mutable data : ('a, 'b) Tiff.Data.t option;
    underlying_area : Area.t;
    mutable active_area : Area.t;
    (* active_area: Area.t; *)
    mutable window : Window.t;
    pixel_scale : Pixel_scale.t;
    tiff_info : tiff_info option;
  }

  let reset_layer layer = 
    layer.data <- None;
    layer.active_area <- layer.underlying_area;
    layer.window <- Window.window 0 0 layer.width (1 * layer.height);
    ()

  let width layer = layer.width
  let height layer = layer.height

  let data layer =
    match layer.data with Some data -> data | None -> raise DataNotReadYet

  let get_data layer = layer.data
  let underlying_area layer = layer.underlying_area
  let active_area layer = layer.active_area
  let window layer = layer.window
  let pixel_scale layer = layer.pixel_scale

  let pp_layer layer =
    Eio.traceln "------------------";
    Eio.traceln "Pretty-printing this layer!";
    Eio.traceln "Width: %i Height: %i" layer.width layer.height;
    Eio.traceln "Underlying area:";
    Area.pp_area layer.underlying_area;
    Eio.traceln "Active area:";
    Area.pp_area layer.active_area;
    Window.pp_window layer.window;
    Pixel_scale.pp_pixel_scale layer.pixel_scale

  let layer_from_file file_name =
    Path.(with_open_in file_name) @@ fun r ->
    let tiff = Tiff.from_file (File.pread_exact r) in
    let ifd = Tiff.ifd tiff in
    let width = Tiff.Ifd.width ifd in
    let height = Tiff.Ifd.height ifd in
    let model_tiepoint = Tiff.Ifd.tiepoint ifd in
    let model_pixel_scale = Tiff.Ifd.pixel_scale ifd in
    let geo_x = model_tiepoint.(3) in
    let geo_y = model_tiepoint.(4) in
    let scale_x = model_pixel_scale.(0) in
    let scale_y = model_pixel_scale.(1) in
    let left = geo_x in
    let top = geo_y in
    let right = geo_x +. (float_of_int width *. scale_x) in
    let bottom = geo_y -. (float_of_int height *. scale_y) in
    let area = Area.area left top right bottom in
    let window = Window.window 0 0 width (1 * height) in
    let pixel_scale = Pixel_scale.pixel_scale scale_x scale_y in
    let tiff_info = Some { file_name = (file_name :> Eio.Fs.dir_ty Eio.Path.t); tiff } in
    {
      width;
      height;
      data = None;
      underlying_area = area;
      active_area = area;
      window;
      pixel_scale;
      tiff_info;
    }

  let set_window layer xoffset yoffset xsize ysize =
    let new_window = Window.window xoffset yoffset xsize ysize in
    layer.window <- new_window

  let update_layer_from_area layer area =
    let underlying_area = layer.underlying_area in
    let pixel_scale = layer.pixel_scale in
    let area_left = Area.left area in
    let area_right = Area.right area in
    let area_top = Area.top area in
    let area_bottom = Area.bottom area in
    let underlying_area_left = Area.left underlying_area in
    let underlying_area_top = Area.top underlying_area in
    let xstep = Pixel_scale.xstep pixel_scale in
    let ystep = Pixel_scale.ystep pixel_scale in
    let xoffset_float =
      Pixel_scale.round_down_pixels
        ((area_left -. underlying_area_left) /. xstep)
        xstep
    in
    let yoffset_float =
      Pixel_scale.round_down_pixels
        ((underlying_area_top -. area_top) /. (ystep *. 1.0))
        (ystep *. 1.0)
    in
    let xsize_float =
      Pixel_scale.round_up_pixels ((area_right -. area_left) /. xstep) xstep
    in
    let ysize_float =
      Pixel_scale.round_up_pixels
        ((area_top -. area_bottom) /. (ystep *. 1.0))
        (ystep *. 1.0)
    in
    let xoffset = int_of_float xoffset_float in
    let yoffset = int_of_float yoffset_float in
    let xsize = int_of_float xsize_float in
    let ysize = int_of_float ysize_float in
    let new_window = Window.window xoffset yoffset xsize ysize in
    layer.active_area <- area;
    layer.window <- new_window

  let empty_layer_like layer =
    let width = width layer in
    let height = height layer in
    let underlying_area = underlying_area layer in
    let original_active_area = active_area layer in
    let active_area = Area.copy_area original_active_area in
    let original_window = window layer in
    let window = Window.copy_window original_window in
    let pixel_scale = pixel_scale layer in
    {
      width;
      height;
      data = None;
      underlying_area;
      active_area;
      window;
      pixel_scale;
      tiff_info = None;
    }

  let rec map f xs = match xs with [] -> [] | x :: xs -> f x :: map f xs

  let pixel_scales_of_layers_are_equal_enough layer_list =
    let pixel_scale_list = map pixel_scale layer_list in
    Pixel_scale.are_pixel_scales_equal_enough pixel_scale_list

  let find_intersection layer_list =
    if not (pixel_scales_of_layers_are_equal_enough layer_list) then
      raise Pixel_scale.PixelScalesNotEqualEnough
    else
      let area_list = map underlying_area layer_list in
      Area.find_intersection_areas area_list

  let find_union layer_list =
    if not (pixel_scales_of_layers_are_equal_enough layer_list) then
      raise Pixel_scale.PixelScalesNotEqualEnough
    else
      let area_list = map underlying_area layer_list in
      Area.find_union_areas area_list

  let read_tiff_layer_data file_name tiff window tiff_data_type =
    Path.(with_open_in file_name) @@ fun r ->
    let xoffset = Window.xoffset window in
    let yoffset = Window.yoffset window in
    let xsize = Window.xsize window in
    let ysize = Window.ysize window in

    let tiff_window = {Tiff.xoff=xoffset; Tiff.yoff=yoffset; Tiff.xsize=xsize; Tiff.ysize=ysize} in

    Tiff.data tiff (File.pread_exact r) ~window:tiff_window tiff_data_type
end

module FloatLayer = struct
  include BaseLayer

  let read_data layer =
    let window = layer.window in
    match layer.tiff_info with
    | Some tiff_info -> (
        let file_name = tiff_info.file_name in
        let tiff = tiff_info.tiff in
        let data =
          read_tiff_layer_data file_name tiff window Tiff.Data.Float32
        in
        layer.data <- Some data)
    | None ->
        (* return a genarray full of zeroes with correct dimensions*)
        let arr_length = Window.xsize window * Window.ysize window in
        let arr = Genarray.create float32 c_layout [| arr_length |] in
        layer.data <- Some arr
end

module UInt8Layer = struct
  include BaseLayer

  let read_data layer =
    let window = layer.window in
    match layer.tiff_info with
    | Some tiff_info -> (
        let file_name = tiff_info.file_name in
        let tiff = tiff_info.tiff in
        let data =
          read_tiff_layer_data file_name tiff window Tiff.Data.Uint8
        in
        layer.data <- Some data)
    | None ->
        (* return a genarray full of zeroes with correct dimensions*)
        let arr_length = Window.xsize window * Window.ysize window in
        let arr = Genarray.create int8_unsigned c_layout [| arr_length |] in
        layer.data <- Some arr
end

module BaseOperationLayer = struct
  exception InvalidArg of string

  type ('a, 'b) operation =
    | ADD_LAYER of ('a, 'b) t
    | MUL_LAYER of ('a, 'b) t
    | ADD_SCALAR of 'a
    | MUL_SCALAR of 'a
    | MAP of ('a -> 'a)
    | IN_LIST of 'a list
    | IN_RANGE of 'a * 'a

  and ('a, 'b) t =
    | SingleLayer of ('a, 'b) BaseLayer.t
    | LayerOperation of ('a, 'b) t * ('a, 'b) operation

  let operation_layer layer = SingleLayer layer

  let add_layer_data lhs rhs =
    let len = Genarray.nth_dim lhs 0 in
    if len <> Genarray.nth_dim rhs 0 then
      raise (InvalidArg "Arrays must have same length")
    else Owl.Dense.Ndarray.Generic.add lhs rhs

  let mul_layer_data lhs rhs =
    let len = Genarray.nth_dim lhs 0 in
    if len <> Genarray.nth_dim rhs 0 then
      raise (InvalidArg "Arrays must have same length")
    else Owl.Dense.Ndarray.Generic.mul lhs rhs

  let add_layer_data_scalar lhs x = Owl.Dense.Ndarray.Generic.add_scalar lhs x
  let mul_layer_data_scalar lhs x = Owl.Dense.Ndarray.Generic.mul_scalar lhs x
  let map_layer_data lhs f = Owl.Dense.Ndarray.Generic.map f lhs

  let in_range_data lhs min_val max_val = 
    let above_min = Owl.Dense.Ndarray.Generic.elt_greater_equal_scalar lhs min_val in 
    let below_min = Owl.Dense.Ndarray.Generic.elt_less_equal_scalar lhs max_val in 
    Owl.Dense.Ndarray.Generic.mul above_min below_min

  let rec dfs_layer layer_operation =
    match layer_operation with
    | SingleLayer layer -> layer
    | LayerOperation (l, _) -> dfs_layer l

  let layer_operation_windows_are_equal_size lhs rhs =
    (* assume windows within lhs and rhs are all of equal size*)
    let lhs_window = BaseLayer.window (dfs_layer lhs) in
    let rhs_window = BaseLayer.window (dfs_layer rhs) in
    Window.windows_are_equal_size lhs_window rhs_window

  let mul lhs x = LayerOperation (lhs, MUL_SCALAR x)
  let add lhs x = LayerOperation (lhs, ADD_SCALAR x)

  let mul_layer lhs rhs =
    (* assume windows within lhs and rhs are all of equal size *)
    if layer_operation_windows_are_equal_size lhs rhs then
      LayerOperation (lhs, MUL_LAYER rhs)
    else raise Window.WindowsAreNotSameSize

  let add_layer lhs rhs =
    (* assume windows within lhs and rhs are all of equal size *)
    if layer_operation_windows_are_equal_size lhs rhs then
      LayerOperation (lhs, ADD_LAYER rhs)
    else raise Window.WindowsAreNotSameSize

  let map lhs f = LayerOperation (lhs, MAP f)
end

module UInt8OperationLayer = struct
  include BaseOperationLayer

  let in_list_layer_data lhs xs = 
    let allowed_tbl = 
      let tbl = Hashtbl.create(List.length xs) in
      List.iter (fun x -> Hashtbl.replace tbl x ()) xs;
      tbl
    in
    let mask = Owl.Dense.Ndarray.Generic.map (fun x -> if Hashtbl.mem allowed_tbl x then 1 else 0) lhs in
    mask

  let eval_single_layer layer =
    let data = UInt8Layer.get_data layer in
    match data with
    | Some data -> data
    | None ->
        UInt8Layer.read_data layer;
        UInt8Layer.data layer

  let rec eval_layer_operation_data layer_operation =
    match layer_operation with
    | SingleLayer layer -> eval_single_layer layer
    | LayerOperation (l, o) -> (
        let left_data = eval_layer_operation_data l in
        match o with
        | ADD_LAYER r ->
            let right_data = eval_layer_operation_data r in
            add_layer_data left_data right_data
        | MUL_LAYER r ->
            let right_data = eval_layer_operation_data r in
            mul_layer_data left_data right_data
        | ADD_SCALAR x -> add_layer_data_scalar left_data x
        | MUL_SCALAR x -> mul_layer_data_scalar left_data x
        | MAP f -> map_layer_data left_data f
        | IN_LIST xs -> in_list_layer_data left_data xs
        | IN_RANGE (min_val, max_val) -> in_range_data left_data min_val max_val)

  let eval_layer_operation layer_operation =
    let result_layer =
      UInt8Layer.empty_layer_like (dfs_layer layer_operation)
    in
    let data = eval_layer_operation_data layer_operation in
    result_layer.data <- Some data;
    result_layer

  let sum_layer layer_operation =
    let res = eval_layer_operation_data layer_operation in
    Owl.Dense.Ndarray.Generic.sum' res

  let binary_filter lhs f =
    (* returns binary masked layer *)
    let g x = if f x then 1 else 0 in
    LayerOperation (lhs, MAP g)

  (* let layer_values_in_list lhs list_of_values =
    (* returns binary masked layer *)
    let f x = List.mem x list_of_values in
    binary_filter lhs f *)

  let layer_values_in_list lhs list_of_values = LayerOperation(lhs, IN_LIST list_of_values)


  (* let layer_values_in_range lhs min max =
    (* returns binary masked layer *)
    (* inclusive of min and max *)
    let f x = x >= min && x <= max in
    binary_filter lhs f *)

    let layer_values_in_range lhs min max = LayerOperation(lhs, IN_RANGE(min, max))

end

module FloatOperationLayer = struct
  include BaseOperationLayer

  let in_list_layer_data lhs xs = 
    let allowed_tbl = 
      let tbl = Hashtbl.create(List.length xs) in
      List.iter (fun x -> Hashtbl.replace tbl x ()) xs;
      tbl
    in
    let mask = Owl.Dense.Ndarray.Generic.map (fun x -> if Hashtbl.mem allowed_tbl x then 1.0 else 0.0) lhs in
    mask
  
  let eval_single_layer layer =
    let data = FloatLayer.get_data layer in
    match data with
    | Some data -> data
    | None ->
        FloatLayer.read_data layer;
        FloatLayer.data layer

  let rec eval_layer_operation_data layer_operation =
    match layer_operation with
    | SingleLayer layer -> eval_single_layer layer
    | LayerOperation (l, o) -> (
        let left_data = eval_layer_operation_data l in
        match o with
        | ADD_LAYER r ->
            let right_data = eval_layer_operation_data r in
            add_layer_data left_data right_data
        | MUL_LAYER r ->
            let right_data = eval_layer_operation_data r in
            mul_layer_data left_data right_data
        | ADD_SCALAR x -> add_layer_data_scalar left_data x
        | MUL_SCALAR x -> mul_layer_data_scalar left_data x
        | MAP f -> map_layer_data left_data f
        | IN_LIST xs -> in_list_layer_data left_data xs
        | IN_RANGE (min_val, max_val) -> in_range_data left_data min_val max_val)

  let eval_layer_operation layer_operation =
    let result_layer =
      FloatLayer.empty_layer_like (dfs_layer layer_operation)
    in
    let data = eval_layer_operation_data layer_operation in
    result_layer.data <- Some data;
    result_layer

  let sum_layer layer_operation =
    let res = eval_layer_operation_data layer_operation in
    Owl.Dense.Ndarray.Generic.sum' res

  let binary_filter lhs f =
    (* returns binary masked layer *)
    let g x = if f x then 1.0 else 0.0 in
    LayerOperation (lhs, MAP g)

  (* let layer_values_in_list lhs list_of_values =
    (* returns binary masked layer *)
    let f x = List.mem x list_of_values in
    binary_filter lhs f *)
  
  let layer_values_in_list lhs list_of_values = LayerOperation(lhs, IN_LIST list_of_values)

  (* let layer_values_in_range lhs min max =
    (* returns binary masked layer *)
    (* inclusive of min and max *)
    let f x = x >= min && x <= max in
    binary_filter lhs f *)

  let layer_values_in_range lhs min max = LayerOperation(lhs, IN_RANGE(min, max))
  
end
