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

type 'a fiber = {
  id: Fiber_handle.t;  (** unique identifier for this fiber *)
  mutable fls: fls_value array;  (** local storage for this fiber *)
  state: 'a fiber_status A.t;
      (** Current state of the fiber (result, or cancellation status) *)
}

and 'a fiber_status =
  | Done of 'a
  | Fail of Exn_bt.t
  | Wait of {
      waiters: 'a fiber_callback list;
          (** Callbacks waiting for the fiber to be done *)
      children: any_fiber FM.t;  (** Set of children *)
    }

and any_fiber = Any_fiber : _ fiber -> any_fiber [@@unboxed]
