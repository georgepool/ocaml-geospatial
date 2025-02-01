type t

val left : t -> float
val top : t -> float
val right : t -> float
val bottom : t -> float
val area : float -> float -> float -> float -> t
val copy_area : t -> t
val pp_area : t -> unit
val intersect_areas : t -> t -> t
val find_intersection_areas : t list -> t
