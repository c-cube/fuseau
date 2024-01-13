(** Effects *)

type _ Effect.t +=
  | (* | Await : (Types.any_fiber_callback -> unit) -> unit Effect.t *)
      Suspend : {
      before_suspend: wakeup:(unit -> unit) -> unit;
          (** This will be called before suspend happens, and it can use the given
              callback to wake up the suspended computation *)
    }
      -> unit Effect.t
  | Yield : unit Effect.t
