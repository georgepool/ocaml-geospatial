open Bigarray

module BaseLayer : sig
    (* type t *)
end

type layer

type pixel_scale = {
  xstep: float;
  ystep: float
}
type layer_data = (int, int8_unsigned_elt,  c_layout) Array1.t

type operation = 
  | ADD_LAYER of layer_operation
  | MUL_LAYER of layer_operation
  | ADD_SCALAR of int
  | MUL_SCALAR of int

and layer_operation = 
| SingleLayer of layer 
| LayerOperation of layer_operation * operation

type area

type window = {
  xoffset: int;
  yoffset: int;
  xsize: int;
  ysize: int
}

val layer_from_file : string -> Tiff.Data.data_type -> layer_operation

val exec_layer : layer_operation -> layer_operation

val width : layer_operation -> int

val height : layer_operation -> int 

val data : layer_operation -> layer_data

val sum_layer : layer_operation -> int

val underlying_area : layer_operation -> area 

val active_area : layer_operation -> area 

val window : layer_operation -> window


val map_layer : layer_operation -> (int -> int) -> layer_operation

val find_intersection : layer_operation list -> area

val set_window_from_area : layer_operation -> area -> window

val empty_layer_like : layer_operation -> layer_operation

val mul : layer_operation -> int -> layer_operation

val add : layer_operation -> int -> layer_operation

val mul_layer : layer_operation -> layer_operation -> layer_operation

val add_layer : layer_operation -> layer_operation -> layer_operation


val eval_layer : layer_operation -> layer_operation

val windows_are_equal : window -> window -> bool

val pp_area : area -> unit

val pp_window : window -> unit


val set_window : layer_operation -> int -> int -> int -> int -> unit