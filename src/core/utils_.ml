open Common_

exception Timeout

let[@inline] get_sched what () : Scheduler.t =
  match !(TLS.get Scheduler.Internal_.k_current_scheduler) with
  | None -> failwith @@ spf "%s must run from inside the fuseau scheduler" what
  | Some s -> s

(** Cancel the current fiber after [delay] seconds, unless
    the fiber terminates first. The cancellation will use
    the {!Timeout} exception. *)
let cancel_after_s (delay : float) =
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
    ignore
      (Event_loop.on_timer loop ~repeat:false delay cancel : Cancel_handle.t)
  ) else
    cancel ()