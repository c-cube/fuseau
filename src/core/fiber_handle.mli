(** The unique name of a fiber *)

type t = private int

val fresh : unit -> t
val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
