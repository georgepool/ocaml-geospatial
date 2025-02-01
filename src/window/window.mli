exception WindowsAreNotSameSize

type t

val xoffset : t -> int
val yoffset : t -> int
val xsize : t -> int
val ysize : t -> int
val window : int -> int -> int -> int -> t
val copy_window : t -> t
val pp_window : t -> unit
val windows_are_equal : t -> t -> bool
