(** Fuseau, a cooperative concurrency library for OCaml 5.
*)

(** {2 Foundations} *)

module Fiber = Fuseau_core.Fiber
module Fiber_handle = Fuseau_core.Fiber_handle
module Main = Fuseau_core.Main
module FLS = Fuseau_core.FLS

(** {2 IO event loop} *)

module Event_loop = Fuseau_core.Event_loop

(** {2 Resource management *)

module Buf_pool = Buf_pool
module Resource_pool = Fuseau_core.Resource_pool

(** {2 Utils} *)

module Exn_bt = Fuseau_core.Exn_bt
module Time = Fuseau_core.Time
module Lock = Fuseau_core.Lock

(** {2 Re-exports} *)

exception Timeout = Utils_.Timeout

let spawn = Fuseau_core.spawn
let spawn_from_anywhere = Fuseau_core.spawn_from_anywhere
let await = Fiber.await
let try_await = Fiber.try_await
let yield = Fiber.yield
let schedule_micro_task = Fuseau_core.schedule_micro_task
let main = Main.main

(** {2 IO primitives} *)

module IO = IO
module IO_in = IO_in
module IO_out = IO_out
module Net = Net

let sleep : float -> unit = Sleep.sleep
let cancel_after = Utils_.cancel_after
