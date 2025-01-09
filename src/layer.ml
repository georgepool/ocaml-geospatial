open Eio
open Bigarray


module BaseLayer = struct
  (* type t = {
    (* data : SomeArrayType; *)
    width : int;
    height : int;
    bounds : float * float * float * float;
  } *)

  (* let create_layer 
  (* ~data  *)
  ~width ~height ~bounds = {
    (* data; *)
    width;
    height;
    bounds
  }

  let width t = t.width
  let height t = t.height
  let bounds t = t.bounds *)


end

type layer_data = (int, int8_unsigned_elt,  c_layout) Array1.t

(* do windoow as area instead 
my issue is the gdal magic thats like "doo doo doo here are these coordinates this is what it looks like pixel-based (i think)"
*)

type pixel_scale = {
  xstep: float;
  ystep: float
}

type area = {
  left: float;
  top: float;
  right: float;
  bottom: float
}

type window = {
  xoffset: int;
  yoffset: int;
  xsize: int;
  ysize: int
}

type layer = {
  width: int;
  height: int;
  data: layer_data;
  underlying_area: area;
  active_area: area;
  window: window;
  pixel_scale: pixel_scale
}

type operation = 
  | ADD_LAYER of layer_operation
  | MUL_LAYER of layer_operation
  | ADD_SCALAR of int
  | MUL_SCALAR of int

and layer_operation = 
| SingleLayer of layer 
| LayerOperation of layer_operation * operation

exception IntersectionDoesNotExist of string;;

exception NestedLayerGivenNotSingleLayer;;

exception WindowsAreNotSameSize;;

exception InvalidArg of string;;
(* let rec exec_layer layer_operation = 
  match layer_operation with
  | SingleLayer(_) -> layer_operation
  | LayerOperation(layer_operation_left, operation(x)) ->
    let left = exec_layer layer_operation_left in 
    match operation with 
    | ADD_LAYER() *)

let windows_are_equal window1 window2 = 
  (window1.xoffset = window2.xoffset) && 
  (window1.yoffset = window2.yoffset) &&
  (window1.xsize = window2.xsize) &&
  (window1.ysize = window2.ysize) 

(* let exec_add_layer left_layer right_layer = 
  match left_layer, right_layer with
  | SingleLayer(left), SingleLayer(right) ->
    if windows_are_equal (left.window) (right.window) then
      blah blah blah
    else 
      raise WindowsAreNotSameSize
  | _ -> raise NestedLayerGivenNotSingleLayer *)

let eval_single_layer layer index =
  let window = layer.window in 
  let xoffset = window.xoffset in 
  let xsize = window.xsize in 
  let data = Array1.sub (layer.data) (index + xoffset) xsize
  data

let add_layer_data lhs rhs index result = 
  let len = Array1.dim lhs in 
  if len <> Array1.dim rhs then
   raise InvalidArg "Arrays must have same length"
  let result = Array1.create (Array1.kind lhs) (Array1.layout lhs) len in
  for i = 0 to len - 1 do
    result.{(i+index)} <- lhs.{i} + rhs.{i}
  done;
  result


let mul_layer_data lhs rhs index result = 
  let len = Array1.dim lhs in 
  if len <> Array1.dim rhs then
    raise InvalidArg "Arrays must have same length"
  let result = Array1.create (Array1.kind lhs) (Array1.layout lhs) len in
  for i = 0 to len - 1 do
    result.{(i+index)} <- lhs.{i} * rhs.{i}
  done;
  result

let add_layer_data_scalar lhs x index result = 
  let result = Array1.create (Array1.kind lhs) (Array1.layout lhs) len in
  for i = 0 to len - 1 do
    result.{(i+index)} <- lhs.{i} + x
  done;
  result

let mul_layer_data_scalar lhs x index result = 
  let result = Array1.create (Array1.kind lhs) (Array1.layout lhs) len in
  for i = 0 to len - 1 do
    result.{(i+index)} <- lhs.{i} * x
  done;
  result

let eval_layer_row layer_operation index_y result_layer =
  (* check if windows are equal? *) 
  (* maybe just have to return an array in the end
  this definitely does not work!!!*)
  match layer_operation with
  | SingleLayer(layer) -> eval_single_layer layer index_y 
  | LayerOperation(l, o) -> 
    let left_data = eval_layer l index_y in 
    let start_index = index_y + result_layer.window.xoffset in 
    match o with 
    | ADD_LAYER(r) ->
      let right_data = eval_layer r index_y in 
      add_layer_data left_data right_data start_index (result_layer.data)
    | MUL_LAYER(r) -> 
      let right_data = eval_layer r index_y in 
      add_layer_data left_data right_data start_index (result_layer.data)
    | ADD_SCALAR(x) -> 
      add_layer_data_scalar left_data x start_index (result_layer.data)
    | MUL_SCALAR(x) ->
      mul_layer_data_scalar right_data x start_index (result_layer.data)
      (* itemwise addition of left and right*)

let rec eval_layer layer_operation output_layer = 
  if windows_are_equal (layer_operation.window) (output_layer.window) then
    let yoffset = ref 0 in
    let ysize = layer_operation.window.ysize in  
    while yoffset < ysize do
      eval_layer_row layer_operation (yoffset * ycount) output_layer.layer;
      yoffset := !yoffset + 1;
    done
  else
    raise WindowsAreNotSameSize
let rec save_layer layer_operation output_layer = 
  match layer_operation with 
  | SingleLayer(_) -> layer_operation
  | LayerOperation(l, o) ->
    let left = exec_layer layer_operation in 
    match o with 
    | ADD_LAYER(r) ->
      let right = exec_layer layer_operation incr_offset
      (*Add layer shit*)
    | MUL_LAYER(r) ->
      let right = exec_layer layer_operation incr_offset
      (*Mul layer shit*)
    | ADD_SCALAR(x) -> 
      blah blah blah
    | MUL_SCALAR(x) -> 
      blah blah blah
        
let rec exec_layer layer_operation = (* absolute draft not finished!!*)
  match layer_operation with
  | SingleLayer(_) -> layer_operation (* should i just switch this to being normal layer again?*)
  | LayerOperation(layer_operation, _) -> exec_layer layer_operation

let rec width layer_operation = 
  match layer_operation with
  | SingleLayer(layer) -> layer.width
  | LayerOperation(layer_operation, _) -> width (exec_layer layer_operation)  (* when exec  layer is right this is right*)

let rec height layer_operation = 
  match layer_operation with
  | SingleLayer(layer) -> layer.height
  | LayerOperation(layer_operation, _) -> height (exec_layer layer_operation)
  
let rec data layer_operation = 
  match layer_operation with
  | SingleLayer(layer) -> layer.data
  | LayerOperation(layer_operation, _) -> data (exec_layer layer_operation)

let rec active_area layer_operation = 
  match layer_operation with
  | SingleLayer(layer) -> layer.active_area
  | LayerOperation(layer_operation, _) -> active_area (exec_layer layer_operation)
  
let rec underlying_area layer_operation = 
  match layer_operation with
  | SingleLayer(layer) -> layer.underlying_area
  | LayerOperation(layer_operation, _) -> underlying_area (exec_layer layer_operation)

let rec window layer_operation = 
  match layer_operation with
  | SingleLayer(layer) -> layer.window
  | LayerOperation(layer_operation, _) -> window (exec_layer layer_operation)


let rec pixel_scale layer_operation = 
  match layer_operation with 
  | SingleLayer(layer) -> layer.pixel_scale
  | LayerOperation(layer_operation, _) -> pixel_scale (exec_layer layer_operation)

  (* let tiff_from_filename file_name = 
    Eio_main.run @@ fun env ->
    let fs = Stdenv.fs env in
    Path.(with_open_in (fs / file_name)) @@ fun r ->
    let tiff = Tiff.from_file (File.pread_exact r) in
    tiff *)

let read_tiff_data ifd ro = 
  let data_offsets = Tiff.Ifd.data_offsets ifd in
  let data_bytecounts = Tiff.Ifd.data_bytecounts ifd in 
  let rows_per_strip = Tiff.Ifd.rows_per_strip ifd in 
  let width = Tiff.Ifd.width ifd in 
  let data = Tiff.read_data2_uint8 ro data_offsets data_bytecounts rows_per_strip width in
  data


let layer_from_file file_name = 
  Eio_main.run @@ fun env ->
    let fs = Stdenv.fs env in
    Path.(with_open_in (fs / file_name)) @@ fun r ->
      let tiff = Tiff.from_file (File.pread_exact r) in
      let ifd = Tiff.ifd tiff in 
      let width = Tiff.Ifd.width ifd in 
      let height = Tiff.Ifd.width ifd in 
      let data = read_tiff_data ifd (File.pread_exact r) in
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
      let area = {left; top; right; bottom} in 
      let window = {
        xoffset=0;
        yoffset=0;
        xsize=width;
        ysize=(-1 * height);
      } in
      let pixel_scale = {
        xstep = scale_x;
        ystep = scale_y
      } in
      let layer = {
        width = width; 
        height = height; 
        data = data; 
        underlying_area = area;
        active_area = area;
        window = window;
        pixel_scale = pixel_scale
      } in
      SingleLayer(layer)

let intersect_areas w1 w2 = 
  let left = max (w1.left) (w2.left) in
  let top = max (w1.top) (w2.top) in
  let right = min (w1.right) (w2.right) in
  let bottom = min (w1.bottom) (w2.bottom) in
  if (left > right || top < bottom) then
    raise (IntersectionDoesNotExist "There is no intersection here")
  else 
    {left; top; right; bottom}

let rec find_intersection_areas_helper area_list area_acc =
  match area_list with 
  | [] -> area_acc
  | w::ws -> find_intersection_areas_helper ws (intersect_areas area_acc w)

let find_intersection_areas area_list = 
  match area_list with
  | [] -> raise (IntersectionDoesNotExist "No intersection of list of 0 areas")
  | w::ws -> find_intersection_areas_helper ws w

let rec map f xs = 
  match xs with 
  | [] -> []
  | x::xs -> (f x) :: (map f xs)

let find_intersection layer_list = 
  let area_list = map active_area layer_list in
  find_intersection_areas area_list

let round_down_pixels value (*pixelscale*) = Float.floor value

let round_up_pixels value (*pixelscale*) = Float.ceil value

let set_window_from_area layer area =
  let layer = exec_layer layer in
  let underlying_area = underlying_area layer in 
  let pixel_scale = pixel_scale layer in
  let xoffset_float = round_down_pixels ((area.left -. underlying_area.left) /. pixel_scale.xstep) (*pixel_scale.xstep*) in
  let yoffset_float = round_down_pixels ((underlying_area.top -. area.top) /. (pixel_scale.ystep *. -1.0)) (*(pixel_scale.ystep *. -1.0)*) in
  let xsize_float = round_up_pixels ((area.right -. area.left) /. pixel_scale.xstep) (*pixel_scale.xstep)*) in
  let ysize_float = round_up_pixels ((area.top -. area.bottom) /. (pixel_scale.ystep *. -1.0)) (*(pixel_scale.ystep *. -1.0)*) in
  let xoffset = int_of_float xoffset_float in 
  let yoffset = int_of_float yoffset_float in
  let xsize = int_of_float xsize_float in 
  let ysize = int_of_float ysize_float in
  {
    xoffset;
    yoffset;
    xsize;
    ysize
  }


let map_layer layer f = (* needs to start considering window stuff!!! *)
  let layer = exec_layer layer in 
  let data = data layer in 
  let length = Array1.dim data in

  for i = 0 to length - 1 do
    let value = Array1.get data i in
    Array1.set data i (f value)
  done;

  let layer = { 
    width=(width layer);
    height=(height layer);
    data=data;
    underlying_area = (underlying_area layer);
    active_area = (active_area layer);
    window=(window layer);
    pixel_scale = (pixel_scale layer)} in 
  SingleLayer(layer)

(* for union I need to check if they are disjoint or whether it comes together *)


let mul lhs x = LayerOperation(lhs, MUL_SCALAR(x))

let mul_layer lhs rhs = LayerOperation(lhs, MUL_LAYER(rhs))  (* assuming that RHS is alreayd a layer operartion *)

let add lhs x = LayerOperation(lhs, ADD_SCALAR(x))

let add_layer lhs rhs = LayerOperation(lhs, ADD_LAYER(rhs))


let empty_layer_like layer_operation =
  let layer = exec_layer layer_operation in
  let width = width layer in
  let height = height layer in
  let length = width * height in
  let data = Array1.create int8_unsigned c_layout length in
  let underlying_area = underlying_area layer in
  let active_area = active_area layer in
  let window = {
    xoffset=0;
    yoffset=0;
    xsize=width;
    ysize=height;
  } in 
  let pixel_scale = pixel_scale layer in
  let new_layer = {
    width = width; 
    height = height; 
    data = data; 
    underlying_area = underlying_area; 
    active_area = active_area; 
    window = window;
    pixel_scale = pixel_scale} in
  SingleLayer(new_layer)


let pp_area area = 
  Eio.traceln "area: left: %f top: %f right: %f bottom: %f" area.left area.top area.right area.bottom
 
let pp_window window = 
  Eio.traceln "window: xoffset: %i yoffset: %i xsize: %i ysize: %i" window.xoffset window.yoffset window.xsize window.ysize

let sum_layer layer_operation = (* needs to consider window! *)
  let l = exec_layer layer_operation in
  let arr = data l in
  let sum = ref 0 in
  for i = 0 to (Array1.dim arr) - 1 do
    sum := !sum + Array1.get arr i
  done;
  !sum


(* TODO: 1. bounds, 2. areas *)