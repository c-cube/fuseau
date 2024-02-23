open Common_

exception Timeout

let[@inline] get_sched what () : Scheduler.t =
  match !(TLS.get Scheduler.k_current_scheduler) with
  | None -> failwith @@ spf "%s must run from inside the fuseau scheduler" what
  | Some s -> s

let cancel_after_s (delay : float) =
  let ebt = Exn_bt.get_callstack 15 Timeout in
  let sched = get_sched "sleep" () in

  let fiber =
    match !Fiber.get_current_ () with
    | None -> failwith "`cancel_after` must be called from a fiber"
    | Some f -> f
  in

  let cancel ev =
    Fiber.cancel_any fiber ebt;
    Cancel_handle.cancel ev
  in

  if delay > 50e-9 then (
    let loop = Scheduler.ev_loop sched in
    ignore
      (Event_loop.on_timer loop ~repeat:false delay cancel : Cancel_handle.t)
  ) else
    cancel Cancel_handle.dummy
