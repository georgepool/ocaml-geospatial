open Eio
open Bigarray


module type LayerElement = sig
  type t
  type layout
  val kind : (t, layout) Bigarray.kind
  val tiff_data_type: Tiff.Data.data_type
  val zero : t
  val add : t -> t -> t
  val mul : t -> t -> t
  val to_string : t -> string
  (* constraint t = int -> tiff_data_type = Tiff.Data.UINT8
  constraint t = float -> tiff_data_type = Tiff.Data.Float *)
end

module UInt8Element = struct
  type t = int
  type layout = int8_unsigned_elt
  let kind = Bigarray.int8_unsigned
  let tiff_data_type = Tiff.Data.UINT8
  let zero = 0
  let add = ( + )
  let mul = ( * )
  let to_string = string_of_int
end

module Float32Element = struct
  type t = float
  type layout = float64_elt
  let kind = Bigarray.float64
  let tiff_data_type = Tiff.Data.FLOAT
  let zero = 0.0
  let add = ( +. )
  let mul = ( *. )
  let to_string = string_of_float
end


module MakeBaseLayer (E : LayerElement) = struct
  type layer_data = (E.t, E.layout, c_layout) Array1.t

  type pixel_scale = {
    xstep: float;
    ystep: float
  }

  type t = {
    width: int;
    height: int;
    data: layer_data;
    underlying_area: Area.t;
    (* mutable*) active_area: Area.t;
    mutable window: Window.t;
    pixel_scale: pixel_scale
  }

  let width layer = layer.width

  let height layer = layer.height

  let data layer = layer.data

  let underlying_area layer = layer.underlying_area

  let active_area layer = layer.active_area

  let window layer = layer.window

  let pixel_scale layer = layer.pixel_scale

  let pp_pixel_scale pixel_scale = 
    Eio.traceln "Pixel Scale: xstep: %f ystep: %f" pixel_scale.xstep pixel_scale.ystep

  let pp_layer layer = 
    Eio.traceln "Width: %i Height: %i" layer.width layer.height;
    Eio.traceln "Underlying area:";
    Area.pp_area layer.underlying_area;
    Eio.traceln "Active area:";
    Area.pp_area layer.active_area;
    Window.pp_window layer.window;
    pp_pixel_scale layer.pixel_scale;;

  let layer_from_file file_name = 
    let data_type = E.tiff_data_type in
    Eio_main.run @@ fun env ->
      let fs = Stdenv.fs env in
      Path.(with_open_in (fs / file_name)) @@ fun r ->
        let tiff = Tiff.from_file (File.pread_exact r) data_type in
        let ifd = Tiff.ifd tiff in 
        let width = Tiff.Ifd.width ifd in 
        let height = Tiff.Ifd.width ifd in 
        let data_wrapper = Tiff.data tiff in
        let data = 
          match data_wrapper with
          | Tiff.Data.UInt8Data(arr) when E.tiff_data_type = Tiff.Data.UINT8 ->
              if Obj.magic E.kind == Bigarray.int8_unsigned then
                (Obj.magic arr : (E.t, E.layout, c_layout) Array1.t)
              else
                raise Tiff.Data.TiffDataHasWrongType
          | Tiff.Data.FloatData(arr) when E.tiff_data_type = Tiff.Data.FLOAT ->
              if Obj.magic E.kind == Bigarray.float64 then
                (Obj.magic arr : (E.t, E.layout, c_layout) Array1.t)
              else
                raise Tiff.Data.TiffDataHasWrongType
          | _ -> raise Tiff.Data.TiffDataHasWrongType
        in
        let model_tiepoint = Tiff.Ifd.tiepoint ifd in
        let model_pixel_scale = Tiff.Ifd.pixel_scale ifd in
        let geo_x = model_tiepoint.(3) in 
        let geo_y = model_tiepoint.(4) in
        let scale_x = model_pixel_scale.(0) in 
        let scale_y = model_pixel_scale.(1) in 
        let left = geo_x in 
        let top = geo_y in 
        let right = geo_x +. ((float_of_int width) *. scale_x) in
        let bottom = geo_y -. ((float_of_int height) *. scale_y) in      
        let area = {Area.left=left; Area.top=top; Area.right=right; Area.bottom=bottom} in 
        let window = {
          Window.xoffset=0;
          Window.yoffset=0;
          Window.xsize=width;
          Window.ysize=(1 * height);
        } in
        let pixel_scale = {
          xstep = scale_x;
          ystep = scale_y
        } in
        {
          width = width; 
          height = height; 
          data = data; 
          underlying_area = area;
          active_area = area;
          window = window;
          pixel_scale = pixel_scale
        }
      
  let round_down_pixels value (*pixelscale*) = Float.floor value

  let round_up_pixels value (*pixelscale*) = Float.ceil value
  
  let set_window layer xoffset yoffset xsize ysize = 
    let new_window = 
      {
        Window.xoffset = xoffset;
        Window.yoffset = yoffset;
        Window.xsize = xsize;
        Window.ysize = ysize
      } in
    layer.window <- new_window;;

  let set_window_from_area layer area =
    let underlying_area = layer.underlying_area in 
    let pixel_scale = layer.pixel_scale in
    let area_left = Area.left area in
    let area_right = Area.right area in 
    let area_top = Area.top area in 
    let area_bottom = Area.bottom area in
    let underlying_area_left = Area.left underlying_area in
    let underlying_area_top = Area.top underlying_area in 
    let xoffset_float = round_down_pixels ((area_left -. underlying_area_left) /. pixel_scale.xstep) (*pixel_scale.xstep*) in
    let yoffset_float = round_down_pixels ((underlying_area_top -. area.top) /. (pixel_scale.ystep *. 1.0)) (*(pixel_scale.ystep *. -1.0)*) in
    let xsize_float = round_up_pixels ((area_right -. area_left) /. pixel_scale.xstep) (*pixel_scale.xstep)*) in
    let ysize_float = round_up_pixels ((area_top -. area_bottom) /. (pixel_scale.ystep *. 1.0)) (*(pixel_scale.ystep *. -1.0)*) in
    let xoffset = int_of_float xoffset_float in 
    let yoffset = int_of_float yoffset_float in
    let xsize = int_of_float xsize_float in 
    let ysize = int_of_float ysize_float in
    {
      Window.xoffset = xoffset;
      Window.yoffset = yoffset;
      Window.xsize = xsize;
      Window.ysize = ysize
    }

  let empty_layer_like layer =
    let width = width layer in
    let height = height layer in
    let length = width * height in
    let data = Array1.create E.kind c_layout length in
    let underlying_area = underlying_area layer in
    let original_active_area = active_area layer in
    let active_area = Area.copy_area original_active_area in
    let original_window = window layer in 
    let window = Window.copy_window original_window in
    let pixel_scale = pixel_scale layer in
    {
      width = width; 
      height = height; 
      data = data; 
      underlying_area = underlying_area; 
      active_area = active_area; 
      window = window;
      pixel_scale = pixel_scale
    }
  
  let rec map f xs =
    match xs with
    | [] -> []
    | x :: xs -> (f x) :: (map f xs)
  
  let find_intersection layer_list = 
    let area_list = map active_area layer_list in 
    Area.find_intersection_areas area_list

  let sum_layer layer = (* needs to consider window! *)
    let arr = data layer in
    let sum = ref E.zero in
    let window = window layer in
    let xsize = Window.xsize window in 
    let ysize = Window.ysize window in 
    let xoffset = Window.xoffset window in 
    let yoffset = Window.yoffset window in 
    let width = width layer in 
    let count = ref 0 in 
    Eio.traceln "Ysize: %i xsize: %i" ysize xsize;
    for y = yoffset to (ysize-1) do
      count := 0;
      for x = xoffset to (xsize-1) do
        let item = Array1.get arr (x + (y * width)) in
        Eio.traceln "Actual value: %s" (E.to_string item);
        sum := (E.add !sum item);
        (* sum := !sum + Array1.get arr (x + (y * width)); *)
        count := !count + 1;
      done;
    done;
    Eio.traceln "Count: %i" !count;
    !sum
  
  let map_layer layer f = (* needs to start considering window stuff!!! *)
    let data = data layer in 
    let length = Array1.dim data in
  
    for i = 0 to length - 1 do
      let value = Array1.get data i in
      Array1.set data i (f value)
    done;

    let original_window = window layer in
    let window = Window.copy_window original_window in
    
    let original_active_area = active_area layer in 
    let active_area = Area.copy_area original_active_area in
    { 
      width = (width layer);
      height = (height layer);
      data = data;
      underlying_area = (underlying_area layer);
      active_area = active_area;
      window = window;
      pixel_scale = (pixel_scale layer)
    }
  
end

module MakeOperationLayer (E : LayerElement) = struct

  exception InvalidArg of string;;

  module BaseLayer = MakeBaseLayer(E)

  type operation = 
    | ADD_LAYER of t
    | MUL_LAYER of t
    | ADD_SCALAR of E.t
    | MUL_SCALAR of E.t

  and t = 
    | SingleLayer of BaseLayer.t
    | LayerOperation of t * operation
  
  let operation_layer layer = SingleLayer(layer)

  let add_layer_data lhs rhs = 
    let len = Array1.dim lhs in 
    if len <> Array1.dim rhs then
      raise (InvalidArg "Arrays must have same length")
    else
      let result = Array1.create (Array1.kind lhs) (Array1.layout lhs) len in
      for i = 0 to (len - 1) do
        result.{i} <- (E.add (lhs.{i})  (rhs.{i}))
      done;
      result

  let mul_layer_data lhs rhs = 
    let len = Array1.dim lhs in 
    if len <> Array1.dim rhs then
      raise (InvalidArg "Arrays must have same length")
    else
      let result = Array1.create (Array1.kind lhs) (Array1.layout lhs) len in
      for i = 0 to (len - 1) do
        result.{i} <- (E.mul (lhs.{i}) (rhs.{i}))
      done;
      result

  let add_layer_data_scalar lhs x = 
    let len = Array1.dim lhs in 
    let result = Array1.create (Array1.kind lhs) (Array1.layout lhs) len in
    for i = 0 to (len - 1) do
      result.{i} <- (E.add (lhs.{i}) x)
    done;
    result

  let mul_layer_data_scalar lhs x = 
    let len = Array1.dim lhs in 
    let result = Array1.create (Array1.kind lhs) (Array1.layout lhs) len in
    for i = 0 to (len - 1) do
      result.{i} <- (E.mul (lhs.{i}) x)
    done;
    result

  let rec dfs_layer layer_operation =
    match layer_operation with
    | SingleLayer(layer) -> layer
    | LayerOperation(l, _) -> dfs_layer l
  
  let eval_single_layer layer index =
    let window = BaseLayer.window layer in 
    let xoffset = Window.xoffset window in 
    let xsize = Window.xsize window in 
    let data = Array1.sub (layer.data) (index + xoffset) xsize in
    data
  
  let rec eval_layer_row layer_operation index_y =
    (* check if windows are equal? *) 
    (* maybe just have to return an array in the end
    this definitely does not work!!!*)
    match layer_operation with
    | SingleLayer(layer) -> eval_single_layer layer index_y 
    | LayerOperation(l, o) -> 
      let left_data = eval_layer_row l index_y in 
      match o with 
      | ADD_LAYER(r) ->
        let right_data = eval_layer_row r index_y in 
        add_layer_data left_data right_data 
      | MUL_LAYER(r) -> 
        let right_data = eval_layer_row r index_y in 
        mul_layer_data left_data right_data
      | ADD_SCALAR(x) -> 
        add_layer_data_scalar left_data x
      | MUL_SCALAR(x) ->
        mul_layer_data_scalar left_data x
        (* itemwise addition of left and right*)
  
  let copy_to_index source_array destination_array index = 
    let source_array_length = Array1.dim source_array in
    let destination_array_length = Array1.dim destination_array in 
  
    if ((index < 0) || (index + source_array_length > destination_array_length)) then 
      raise (InvalidArg "Index out of bounds / destination array is too small")
    else
      for j = 0 to source_array_length - 1 do
        Array1.set destination_array (index + j) (Array1.get source_array j)
      done;;

  let eval_layer_operation layer_operation = (* does not work when the files are not the same size *)
    let layer = dfs_layer layer_operation in
    let output_layer = BaseLayer.empty_layer_like layer in (* this should maybe do its own thing *)
    let output_layer_data = BaseLayer.data output_layer in
    let yoffset = ref 0 in
    (* Window will be same for all layers in operation*)
    let window = BaseLayer.window layer in 
    let ysize = Window.ysize window in
    let width = BaseLayer.width layer in  
    let xoffset = Window.xoffset window in
    let ycount = ref 0 in 
    while !ycount < ysize do
      yoffset := !ycount * width;
      let result_data = eval_layer_row layer_operation !yoffset in
      ycount := !ycount + 1;
      copy_to_index result_data output_layer_data (!yoffset + xoffset);
    done;
    output_layer

  let layer_operation_windows_are_equal lhs rhs= 
    (* assume windows within lhs and rhs are all equal*)
    let lhs_window = BaseLayer.window (dfs_layer lhs) in
    let rhs_window = BaseLayer.window (dfs_layer rhs) in  
    Window.windows_are_equal lhs_window rhs_window 
    
  let mul lhs x = LayerOperation(lhs, MUL_SCALAR(x))

  let add lhs x = LayerOperation(lhs, ADD_SCALAR(x))

  let mul_layer lhs rhs = 
    if layer_operation_windows_are_equal lhs rhs then
      LayerOperation(lhs, MUL_LAYER(rhs))  (* assuming that RHS is alreayd a layer operartion *)
    else 
      raise Window.WindowsAreNotSameSize
  
  let add_layer lhs rhs =
    if layer_operation_windows_are_equal lhs rhs then
      LayerOperation(lhs, ADD_LAYER(rhs))
    else
      raise Window.WindowsAreNotSameSize

end

module UInt8BaseLayer = MakeBaseLayer(UInt8Element)

module UInt8OperationLayer = MakeOperationLayer(UInt8Element)
