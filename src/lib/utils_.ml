open Common_
open Fuseau_core

exception Timeout

let cancel_after (delay : float) =
  let ebt = Exn_bt.get_callstack 15 Timeout in
  let sched = get_sched "sleep" () in

  let fiber =
    match !Fiber.Internal_.get_current () with
    | None -> failwith "cancel_after must be called from a fiber"
    | Some f -> f
  in

  let cancel _ = Fiber.Internal_.cancel_any fiber ebt in

  if delay > 50e-9 then (
    let loop = Scheduler.Internal_.ev_loop sched in
    ignore (Event_loop.on_timer loop ~repeat:false delay cancel : event_handle)
  ) else
    cancel ()
