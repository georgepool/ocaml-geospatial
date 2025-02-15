open Bigarray

module UInt8Layer : sig
  type tiff_info
  type ('a, 'b) t

  val width : (int, int8_unsigned_elt) t -> int
  val height : (int, int8_unsigned_elt) t -> int
  val underlying_area : (int, int8_unsigned_elt) t -> Area.t

  val data :
    (int, int8_unsigned_elt) t -> (int, int8_unsigned_elt) Tiff.Data.tiff_data

  val window : (int, int8_unsigned_elt) t -> Window.t
  val pixel_scale : (int, int8_unsigned_elt) t -> Pixel_scale.t
  val pp_layer : (int, int8_unsigned_elt) t -> unit
  val read_data : (int, int8_unsigned_elt) t -> unit
  val layer_from_file : string -> (int, int8_unsigned_elt) t

  val set_window :
    (int, int8_unsigned_elt) t -> int -> int -> int -> int -> unit

  val update_layer_from_area : (int, int8_unsigned_elt) t -> Area.t -> unit

  val empty_layer_like :
    (int, int8_unsigned_elt) t -> (int, int8_unsigned_elt) t

  val find_intersection : (int, int8_unsigned_elt) t list -> Area.t
  val find_union : (int, int8_unsigned_elt) t list -> Area.t
end

module FloatLayer : sig
  type tiff_info
  type ('a, 'b) t

  val width : (float, float32_elt) t -> int
  val height : (float, float32_elt) t -> int
  val underlying_area : (float, float32_elt) t -> Area.t
  val window : (float, float32_elt) t -> Window.t
  val pixel_scale : (float, float32_elt) t -> Pixel_scale.t
  val pp_layer : (float, float32_elt) t -> unit
  val data : ('a, 'b) t -> ('a, 'b) Tiff.Data.tiff_data
  val read_data : (float, float32_elt) t -> unit
  val layer_from_file : string -> (float, float32_elt) t
  val set_window : (float, float32_elt) t -> int -> int -> int -> int -> unit
  val update_layer_from_area : (float, float32_elt) t -> Area.t -> unit
  val empty_layer_like : (float, float32_elt) t -> (float, float32_elt) t
  val find_intersection : (float, float32_elt) t list -> Area.t
  val find_union : (float, float32_elt) t list -> Area.t
end

module UInt8OperationLayer : sig
  exception InvalidArg of string

  type ('a, 'b) operation =
    | ADD_LAYER of ('a, 'b) t
    | MUL_LAYER of ('a, 'b) t
    | ADD_SCALAR of 'a
    | MUL_SCALAR of 'a
    | MAP of ('a -> 'a)

  and ('a, 'b) t =
    | SingleLayer of ('a, 'b) UInt8Layer.t
    | LayerOperation of ('a, 'b) t * ('a, 'b) operation

  val operation_layer :
    (int, int8_unsigned_elt) UInt8Layer.t -> (int, int8_unsigned_elt) t

  val eval_layer_operation :
    (int, int8_unsigned_elt) t -> (int, int8_unsigned_elt) UInt8Layer.t

  val mul : (int, int8_unsigned_elt) t -> int -> (int, int8_unsigned_elt) t
  val add : (int, int8_unsigned_elt) t -> int -> (int, int8_unsigned_elt) t

  val mul_layer :
    (int, int8_unsigned_elt) t ->
    (int, int8_unsigned_elt) t ->
    (int, int8_unsigned_elt) t

  val add_layer :
    (int, int8_unsigned_elt) t ->
    (int, int8_unsigned_elt) t ->
    (int, int8_unsigned_elt) t

  val map :
    (int, int8_unsigned_elt) t -> (int -> int) -> (int, int8_unsigned_elt) t

  val sum_layer : (int, int8_unsigned_elt) t -> int

  val binary_filter :
    (int, int8_unsigned_elt) t -> (int -> bool) -> (int, int8_unsigned_elt) t

  val layer_values_in_list :
    (int, int8_unsigned_elt) t -> int list -> (int, int8_unsigned_elt) t

  val layer_values_in_range :
    (int, int8_unsigned_elt) t -> int -> int -> (int, int8_unsigned_elt) t

end

module FloatOperationLayer : sig
  exception InvalidArg of string

  type ('a, 'b) operation =
    | ADD_LAYER of ('a, 'b) t
    | MUL_LAYER of ('a, 'b) t
    | ADD_SCALAR of 'a
    | MUL_SCALAR of 'a
    | MAP of ('a -> 'a)

  and ('a, 'b) t =
    | SingleLayer of ('a, 'b) FloatLayer.t
    | LayerOperation of ('a, 'b) t * ('a, 'b) operation

  val operation_layer :
    (float, float32_elt) FloatLayer.t -> (float, float32_elt) t

  val eval_layer_operation :
    (float, float32_elt) t -> (float, float32_elt) FloatLayer.t

  val mul : (float, float32_elt) t -> float -> (float, float32_elt) t
  val add : (float, float32_elt) t -> float -> (float, float32_elt) t

  val mul_layer :
    (float, float32_elt) t -> (float, float32_elt) t -> (float, float32_elt) t

  val add_layer :
    (float, float32_elt) t -> (float, float32_elt) t -> (float, float32_elt) t

  val map : (float, float32_elt) t -> (float -> float) -> (float, float32_elt) t
  val sum_layer : (float, float32_elt) t -> float

  val binary_filter :
    (float, float32_elt) t -> (float -> bool) -> (float, float32_elt) t

  val layer_values_in_list :
    (float, float32_elt) t -> float list -> (float, float32_elt) t

  val layer_values_in_range :
    (float, float32_elt) t -> float -> float -> (float, float32_elt) t

end
