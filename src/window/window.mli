exception WindowsAreNotSameSize

type t = { xoffset : int; yoffset : int; xsize : int; ysize : int }

val xoffset : t -> int
val yoffset : t -> int
val xsize : t -> int
val ysize : t -> int
val copy_window : t -> t
val pp_window : t -> unit
val windows_are_equal : t -> t -> bool
