(*
Authors: Jan Dudek, Michael Dales
*)
val decode : Bytes.t -> int -> Bytes.t
val encode : Bytes.t -> int -> Bytes.t
val flatten_codes : ?pad:bool -> int -> (Z.t * int) list -> Bytes.t
val get_bits : Bytes.t -> int -> int -> int
