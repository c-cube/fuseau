(** Some type definitions *)

open Common_
module FM = Fiber_handle.Map

type fls_value = ..

module type FLS_KEY = sig
  type t
  type fls_value += V of t

  val offset : int
  (** Unique offset *)

  val init : unit -> t
end

type 'a fls_key = (module FLS_KEY with type t = 'a)
(** A FLS key (fiber local storage) *)

type 'a fiber_callback = ('a, Exn_bt.t) result -> unit
type cancel_callback = Exn_bt.t -> unit

type 'a fiber = {
  id: Fiber_handle.t;  (** unique identifier for this fiber *)
  mutable fls: fls_value array;  (** local storage for this fiber *)
  name: string;  (** Optional name, for tracing *)
  state: 'a fiber_status A.t;
      (** Current state of the fiber (result, or cancellation status) *)
}

and 'a fiber_status =
  | Done of 'a Exn_bt.result
  | Wait of {
      already_done: 'a Exn_bt.result option;
          (** If [Some _], the main computation has ended but we're
          waiting for children to terminate. This means we can't
          start new children. We can still change our mind and turn
          [Some (Ok _)] into [Some (Error ebt)] if a child fails. *)
      waiters: 'a fiber_callback list;
          (** Callbacks waiting for the fiber to be done *)
      children: any_fiber FM.t;  (** Set of children *)
      on_cancel: cancel_callback list;
    }

and any_fiber = Any_fiber : _ fiber -> any_fiber [@@unboxed]
