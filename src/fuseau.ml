module Buffer = Buffer
module Computation = Computation
module Disposable = Disposable
module Err = Err
module Event_loop = Event_loop
module Exn_bt = Exn_bt
module Fiber = Fiber
module Stream = Stream
module Tcp = Tcp
module Timer = Timer
module Scheduler = Scheduler
module Trigger = Trigger

(** See {!Event_loop.main} *)
let main = Event_loop.main

(** See {!Scheduler.spawn} *)
let spawn = Scheduler.spawn

(** See {!Scheduler.spawn_from_anywhere} *)
let spawn_from_anywhere = Scheduler.spawn_from_anywhere
