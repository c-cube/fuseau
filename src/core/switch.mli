(** Cancellation switch.

 The switch is a building block for structured concurrency. It
 coordinates cancellation for multiple fibers in the same scope. *)

type t = Types.switch

val done_ : t
(** A switch that is already done. *)

val create_root : unit -> t
(** Create a new root switch. It has no parent and thus is
    only cancelled from the inside or explicitly. *)

val create_sub : parent:t -> propagate_cancel_to_parent:bool -> unit -> t
(** Create a sub-switch that is cancelled from the inside
    or when its parent is cancelled.
    @param propagate_cancel_to_parent if true, cancelling this switch
    will also cancel its parent. If [false], failure is contained. *)

val cancel : t -> Exn_bt.t -> unit
val is_done : t -> bool

exception Cancel
(** Exception raised in case a switch is cancelled.
    A fiber can also raise this in order to cancel its surrounding switch. *)

(**/**)

module Internal_ : sig
  val add_child : t -> Types.any_fiber -> unit
  val remove_child : t -> Types.any_fiber -> unit
end

(**/**)
