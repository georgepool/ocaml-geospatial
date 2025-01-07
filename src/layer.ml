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
  area: area;
  window: window
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

let rec area layer_operation = 
  match layer_operation with
  | SingleLayer(layer) -> layer.area
  | LayerOperation(layer_operation, _) -> area (exec_layer layer_operation)

let rec window layer_operation = 
  match layer_operation with
  | SingleLayer(layer) -> layer.window
  | LayerOperation(layer_operation, _) -> window (exec_layer layer_operation)

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

let calculating_area ifd = 
  let width = float_of_int (Tiff.Ifd.width ifd) in
  let height = float_of_int (Tiff.Ifd.height ifd) in 
  let model_tiepoint = Tiff.Ifd.tiepoint ifd in
  let model_pixel_scale = Tiff.Ifd.pixel_scale ifd in
  let geo_x = model_tiepoint.(3) in 
  let geo_y = model_tiepoint.(4) in
  let scale_x = model_pixel_scale.(0) in 
  let scale_y = model_pixel_scale.(1) in 
  let left = geo_x in 
  let top = geo_y in 
  let right = geo_x +. (width *. scale_x) in
  let bottom = geo_y -. (height *. scale_y) in
  {left; top; right; bottom}
      
let layer_from_file file_name = 
  Eio_main.run @@ fun env ->
    let fs = Stdenv.fs env in
    Path.(with_open_in (fs / file_name)) @@ fun r ->
      let tiff = Tiff.from_file (File.pread_exact r) in
      let ifd = Tiff.ifd tiff in 
      let width = Tiff.Ifd.width ifd in 
      let height = Tiff.Ifd.width ifd in 
      let data = read_tiff_data ifd (File.pread_exact r) in
      let area = calculating_area ifd in 
      let window = {
        xoffset=0;
        yoffset=0;
        xsize=width;
        ysize=height;
      } in
      let layer = {width; height; data; area; window} in
      SingleLayer(layer)

let intersect_areas w1 w2 = 
  let left = max (w1.left) (w2.left) in
  let top = max (w1.top) (w2.top) in
  let right = min (w1.right) (w2.right) in
  let bottom = min (w1.bottom) (w2.bottom) in
  if (left >= right || top >= bottom) then
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
  let area_list = map area layer_list in
  find_intersection_areas area_list
  

let map_layer layer f = 
  let data = layer.data in 
  let length = Array1.dim data in
  for i = 0 to length - 1 do
    let value = Array1.get data i in
    Array1.set data i (f value)
  done;
  let layer = { width=layer.width;
    height=layer.height;
    data=data;
    area=layer.area;
    window=layer.window} in 
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
  let area = area layer in
  let window = {
    xoffset=0;
    yoffset=0;
    xsize=width;
    ysize=height;
  } in 
  let new_layer = {width; height; data; area; window} in
  SingleLayer(new_layer)


let pp_area area = 
  Eio.traceln "area: left: %f top: %f right: %f bottom: %f" area.left area.top area.right area.bottom
 
let sum_layer layer_operation = 
  let l = exec_layer layer_operation in
  let arr = data l in
  let sum = ref 0 in
  for i = 0 to (Array1.dim arr) - 1 do
    sum := !sum + Array1.get arr i
  done;
  !sum


(* TODO: 1. bounds, 2. areas *)