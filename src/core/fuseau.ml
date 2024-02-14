(** {2 Foundations} *)

module Fiber = Fiber
module Fiber_handle = Fiber_handle
module Main = Main
module FLS = FLS
module Event = Event

type 'ret branch = 'ret Event.branch =
  | When : 'a Event.t * ('a -> 'ret) -> 'ret branch

let select = Event.select

(** {2 IO event loop} *)

exception Inactive = Scheduler.Inactive

module Scheduler = Scheduler
module Event_loop = Event_loop

(** {2 Resource management *)

module Resource_pool = Resource_pool
module Buf_pool = Buf_pool
module Cancel_handle = Cancel_handle

(** {2 Utils} *)

module Exn_bt = Exn_bt
module Time = Time

(** {2 Synchronization} *)

module Chan = Chan

(** {2 IO} *)

module Iostream = Iostream

(** {2 Sleep} *)

module Sleep = Sleep

(** {2 Re-exports} *)

exception Timeout = Utils_.Timeout
(** Exception used for cancellation caused by timeout *)

let await = Fiber.await
let try_await = Fiber.try_await
let cancel_after_s = Utils_.cancel_after_s
let with_cancel_callback = Fiber.with_cancel_callback
let yield = Fiber.yield
let spawn = Scheduler.spawn
let spawn_from_anywhere = Scheduler.spawn_from_anywhere
let spawn_as_child_of = Scheduler.spawn_as_child_of
let schedule_micro_task = Scheduler.schedule_micro_task
let main = Main.main
let sleep_s = Sleep.sleep_s
let ev_timeout = Sleep.ev_deadline
let ev_deadline = Sleep.ev_deadline
let get_scheduler = Utils_.get_sched "get_scheduler"

module Private_ = struct
  module TLS = Common_.TLS

  let suspend = Fiber.suspend
end
