type t = {
  left: float;
  top: float;
  right: float;
  bottom: float
}

val left : t -> float

val top : t -> float

val right : t -> float

val bottom : t -> float

val copy_area : t -> t 

val pp_area : t -> unit

val intersect_areas : t -> t -> t

val find_intersection_areas : t list -> t
