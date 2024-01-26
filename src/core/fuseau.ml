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

module Scheduler = Scheduler
module Event_loop = Event_loop

(** {2 Resource management *)

module Resource_pool = Resource_pool
module Buf_pool = Buf_pool
module Cancel_handle = Cancel_handle

(** {2 Utils} *)

module Exn_bt = Exn_bt
module Time = Time
module Lock = Lock

(** {2 IO} *)

module IO = IO
module IO_in = IO_in
module IO_out = IO_out

(** {2 Networking} *)

module Net = Net

(** {2 Sleep} *)

module Sleep = Sleep

(** {2 Re-exports} *)

exception Timeout = Utils_.Timeout
(** Exception used for cancellation caused by timeout *)

let await = Fiber.await
let try_await = Fiber.try_await
let cancel_after_s = Utils_.cancel_after_s
let spawn = Scheduler.spawn
let spawn_from_anywhere = Scheduler.spawn_from_anywhere
let spawn_as_child_of = Scheduler.spawn_as_child_of
let schedule_micro_task = Scheduler.schedule_micro_task
let main = Main.main
let sleep_s = Sleep.sleep_s