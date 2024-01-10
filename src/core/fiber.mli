(** Fibers.

    A fiber is a lightweight cooperative thread.
    It runs on the scheduler and can yield to other fibers,
    notably on IO polling points.
*)

type 'a t = 'a Types.fiber
(** A fiber returning a value of type ['a]. *)

(** A fiber with erased return type. *)
type any = Types.any_fiber = Any_fiber : _ t -> any [@@unboxed]

type 'a callback = 'a Exn_bt.result -> unit
(** Callbacks that are called when a fiber is done. *)

type 'a state = 'a Types.fiber_status =
  | Done of 'a
  | Fail of Exn_bt.t
  | Wait of { waiters: 'a callback list }

val peek : 'a t -> 'a state
val await : 'a t -> 'a
val is_cancelled : _ t -> bool

(* val try_await : 'a t -> 'a Exn_bt.result *)

val on_res : 'a t -> 'a callback -> unit
(** Wait for fiber to be done and call the callback
    with the result. *)

(**/**)

module Internal_ : sig
  val create : switch:Types.switch -> unit -> 'a t
  val resolve : 'a t -> 'a -> unit
  val cancel : _ t -> Exn_bt.t -> unit
  val get_current : (unit -> any option) ref
  val switch : _ t -> Types.switch
  val switch_any : any -> Types.switch

  val suspend : before_suspend:(wakeup:(unit -> unit) -> unit) -> unit
  (** [suspend ~before_suspend] first calls [before_suspend] with a wakeup
      function, and then suspends the current fiber.
      When the wakeup function is called, the fiber resumes. *)
end

(**/**)
