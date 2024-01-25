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
  | Wait of {
      waiters: 'a callback list;
      children: any Fiber_handle.Map.t;
      on_cancel: Types.cancel_callback list;
    }

val peek : 'a t -> 'a state
val is_cancelled : _ t -> bool
val is_done : _ t -> bool

exception Cancelled of Exn_bt.t
(** Exception for fibers that are cancelled. Polling points such
    as {!yield} and {!await} will raise this if the fiber has been cancelled. *)

val yield : unit -> unit
(** [yield ()] returns control to the scheduler and checks for
    cancellation. *)

val await : 'a t -> 'a
(** Wait for the fiber to terminate, and return the result.
    If the fiber failed, this re-raises the exception.

    This must be called from inside another fiber, which will be
    suspended if needed. *)

val try_await : 'a t -> 'a Exn_bt.result

val on_res : 'a t -> 'a callback -> unit
(** Wait for fiber to be done and call the callback
    with the result. If the fiber is done already then the
    callback is invoked immediately with its result. *)

val with_cancel_callback : (Exn_bt.t -> unit) -> (unit -> 'a) -> 'a
(** [let@ () = with_cancel_callback cb in <e>] evaluates [e]
    in a scope in which, if the current fiber is cancelled,
    [cb()] is called. If [e] returns without the fiber being cancelled,
    this callback is removed. *)

(**/**)

module Internal_ : sig
  val create : ?name:string -> unit -> 'a t
  val resolve : 'a t -> 'a -> unit
  val cancel : _ t -> Exn_bt.t -> unit
  val cancel_any : any -> Exn_bt.t -> unit
  val get_current : (unit -> any option) ref
  val add_child : protected:bool -> _ t -> _ t -> unit

  val suspend : before_suspend:(wakeup:(unit -> unit) -> unit) -> unit
  (** [suspend ~before_suspend] first calls [before_suspend] with a wakeup
      function, and then suspends the current fiber.
      When the wakeup function is called, the fiber resumes. *)
end

(**/**)
