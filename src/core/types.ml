(** Some type definitions *)

open Common_

type 'a fiber_callback = ('a, Exn_bt.t) result -> unit

type any_fiber_callback =
  | Any_fiber_callback : _ fiber_callback -> any_fiber_callback
[@@unboxed]

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

type switch = { state: switch_status A.t } [@@unboxed]

and switch_status =
  | Active of {
      children: any_fiber Fiber_handle.Map.t;
          (** Fibers started directly under this switch *)
      waiters: switch_callback list;
    }
  | Done
  | Cancelled of Exn_bt.t

and switch_callback = (unit, Exn_bt.t) result -> unit
(** Callback for when the switch terminates *)

and 'a fiber_status =
  | Done of 'a
  | Fail of Exn_bt.t
  | Wait of { waiters: 'a fiber_callback list }

and 'a fiber = {
  id: Fiber_handle.t;  (** unique identifier for this fiber *)
  switch: switch;  (** Switch this fiber belongs to *)
  mutable fls: fls_value array;  (** local storage for this fiber *)
  state: 'a fiber_status A.t;
      (** Current state of the fiber (result, or cancellation status) *)
}

and any_fiber = Any_fiber : _ fiber -> any_fiber [@@unboxed]
