(** Fuseau core.

    The core library contains the definition of the
    main scheduler, fibers, switches, and other foundations
    for cooperative structured concurrency.
*)

(** {2 Foundations} *)

module Fiber = Fiber
module Fiber_handle = Fiber_handle
module Main = Main
module FLS = FLS

(** {2 IO event loop} *)

module Event_loop = Event_loop

(** {2 Resource management *)

module Buf_pool = Buf_pool
module Resource_pool = Resource_pool

(** {2 Utils} *)

module Exn_bt = Exn_bt
module Time = Time
module Lock = Lock

(** {2 Re-exports} *)

let spawn = Scheduler.spawn
let spawn_from_anywhere = Scheduler.spawn_from_anywhere
let schedule_micro_task = Scheduler.schedule_micro_task
let main = Main.main
