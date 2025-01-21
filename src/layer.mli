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
end

module UInt8Element : sig
  type t = int
  type layout = int8_unsigned_elt
  val kind : (int, int8_unsigned_elt) Bigarray.kind
  val tiff_data_type : Tiff.Data.data_type
  val zero : int
  val add : int -> int -> int
  val mul : int -> int -> int
  val to_string : int -> string
end

module Float32Element : sig 
  type t = float
  type layout = float64_elt
  val kind : (float, float64_elt) Bigarray.kind
  val tiff_data_type : Tiff.Data.data_type
  val zero : float
  val add : float -> float -> float
  val mul : float -> float -> float
  val to_string : float -> string
end

module MakeBaseLayer (E : LayerElement) : sig

  type layer_data = (E.t, E.layout, c_layout) Array1.t

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

  val layer_from_file : string -> t
    
  val set_window : t -> int -> int -> int -> int -> unit

  val set_window_from_area : t -> Area.t -> Window.t

  val empty_layer_like : t -> t 

  val find_intersection : t list -> Area.t

  val sum_layer : t -> E.t

  val map_layer : t -> (E.t -> E.t) -> t

end

module MakeOperationLayer (E : LayerElement) : sig

  type operation

  type t

  module BaseLayer : module type of MakeBaseLayer(E)

  val operation_layer : BaseLayer.t -> t

  val eval_layer_operation : t -> BaseLayer.t

  val mul : t -> E.t -> t

  val add : t -> E.t -> t

  val mul_layer : t -> t -> t

  val add_layer : t -> t -> t 

end

module UInt8BaseLayer : module type of MakeBaseLayer(UInt8Element)

module UInt8OperationLayer : module type of MakeOperationLayer(UInt8Element)

(* module UInt8Layer : sig 
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
end *)