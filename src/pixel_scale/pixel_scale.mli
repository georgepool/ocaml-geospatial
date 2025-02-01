exception PixelScalesNotEqualEnough

type t

val xstep : t -> float
val ystep : t -> float
val pixel_scale : float -> float -> t
val pp_pixel_scale : t -> unit
val round_down_pixels : float -> float -> float
val round_up_pixels : float -> float -> float
val are_pixel_scales_equal_enough : t list -> bool
