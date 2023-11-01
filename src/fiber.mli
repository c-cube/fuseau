(** Fibers.

   A fiber is a logical thread of execution. *)

type 'a t

val equal : _ t -> _ t -> bool
val spawn : (unit -> 'a) -> 'a t
val res : 'a t -> 'a Fut.t
val await : 'a t -> 'a

(**/**)

module Private : sig
  type _ Effect.t +=
    | Spawn :
        (unit -> unit)
        -> unit Effect.t (* TODO: take bundle as arg, return fiber as result *)

  val create : unit -> 'a t * 'a Fut.promise
end

(**/**)
