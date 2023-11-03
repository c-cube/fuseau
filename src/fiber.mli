(** Fibers.

   A fiber is a logical thread of execution. *)

type 'a t

val equal : 'a t -> 'a t -> bool
val spawn : (unit -> 'a) -> 'a t
val await : 'a t -> 'a
val cancel : _ t -> Exn_bt.t -> unit
val is_done : _ t -> bool
val check : _ t -> unit
val forbid_cancellation : _ t -> (unit -> 'a) -> 'a
val permit_cancellation : _ t -> (unit -> 'a) -> 'a

(*
TODO: this requires help from picos?

exception Not_done

val res_or_fail_exn : 'a t -> 'a
(** Get result immediately or fail.
    @raise Not_done if the computation is not done. *)
*)

module FLS = Picos.Fiber.FLS

(**/**)

module Private : sig
  val create : unit -> 'a t
  val comp : 'a t -> ('a, [ `Await | `Cancel | `Return ]) Picos.Computation.t
end

(**/**)
