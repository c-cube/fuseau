(** Fibers.

   A fiber is a logical thread of execution. *)

type 'a t

val spawn : (unit -> 'a) -> 'a t
val await : 'a t -> 'a
