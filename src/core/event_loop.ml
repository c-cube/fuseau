(** Abstraction over an event loop *)

open Common_

type event_handle = Event_loop_types.event
(** An event handle, so we can cancel events *)

type file_descr = Event_loop_types.file_descr
(** File descriptors for input/output *)

(* FIXME: a special method to wakeup from the outside (thread safe).
   E.g. use a hidden FD, like a pipe, and write a byte to it. *)

(** Abstract event loop, inspired by Lwt engine *)
class type t =
  object
    method one_step : block:bool -> unit -> unit
    (** Run one step of the event loop.
        @param block if [true], the call might block until the next timeout
        or until the next IO event occurs. If [false], this does not
        block and returns after having processed the available events. *)

    method on_readable : file_descr -> (event_handle -> unit) -> event_handle
    (** [on_readable fd f] creates a new event [ev], and will run [f ev] when
      [fd] becomes readable *)

    method on_writable : file_descr -> (event_handle -> unit) -> event_handle

    method on_timer :
      float -> repeat:bool -> (event_handle -> unit) -> event_handle
    (** [on_timer delay ~repeat f] runs [f] after [delay].
      @param repeat if true runs [f] every [delay] seconds *)

    method fake_io : file_descr -> unit
    (** Simulate activity on the FD *)

    method readable_count : int
    (** Number of events waiting for FDs to be readable FDs *)

    method writable_count : int

    method timer_count : int
    (** Number of events waiting on a timer *)
  end

let has_pending_tasks (self : #t) : bool =
  self#readable_count > 0 || self#writable_count > 0 || self#timer_count > 0

let one_step (self : #t) ~block () = self#one_step ~block ()
let on_readable (self : #t) fd f = self#on_readable fd f
let on_writable (self : #t) fd f = self#on_writable fd f
let on_timer (self : #t) delay ~repeat f = self#on_timer delay ~repeat f
let fake_io (self : #t) fd = self#fake_io fd
let readable_count (self : #t) = self#readable_count
let writable_count (self : #t) = self#writable_count
let timer_count (self : #t) = self#timer_count
