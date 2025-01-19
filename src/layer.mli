open Bigarray

module BaseLayer : sig

  type layer_data = (int, int8_unsigned_elt,  c_layout) Array1.t

  type pixel_scale

  type t

  val width : t -> int

  val height : t -> int

  val data : t -> layer_data

  val underlying_area : t -> Area.t

  val active_area : t -> Area.t

  val window : t -> Window.t

  val pixel_scale :  t -> pixel_scale

  val pp_pixel_scale : pixel_scale -> unit

  val pp_layer : t -> unit

  val layer_from_file : string -> Tiff.Data.data_type -> t

  val set_window : t -> int -> int -> int -> int -> unit

  val set_window_from_area : t -> Area.t -> Window.t

  val empty_layer_like : t -> t 

  val find_intersection : t list -> Area.t

  val sum_layer : t -> int

  val map_layer : t -> (int -> int) -> t

end

module OperationLayer : sig
  
  type operation

  type t

  val operation_layer : BaseLayer.t -> t

  val eval_layer_operation : t -> BaseLayer.t

  val mul : t -> int -> t

  val add : t -> int -> t

  val mul_layer : t -> t -> t

  val add_layer : t -> t -> t

end
