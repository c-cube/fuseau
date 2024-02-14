open Utils_

let sleep_s delay : unit =
  if delay > 50e-9 then (
    let sched = get_sched "sleep" () in
    let loop = Scheduler.ev_loop sched in
    Fiber.suspend ~before_suspend:(fun ~wakeup ->
        ignore
          (Event_loop.on_timer loop ~repeat:false delay (fun ev ->
               wakeup ();
               Cancel_handle.cancel ev)
            : Cancel_handle.t);
        ())
  )

let ev_deadline deadline : _ Event.t =
  let poll () =
    if Time.monotonic_time_s () >= deadline then
      Some (Error (Exn_bt.get Utils_.Timeout))
    else
      None
  in

  let wait cb =
    let now = Time.monotonic_time_s () in
    if now +. 0.000_005 < deadline then (
      let sched = Utils_.get_sched "sleep" () in
      let loop = Scheduler.ev_loop sched in
      let delay = deadline -. now in
      Event_loop.on_timer loop ~repeat:false delay (fun ev ->
          cb ();
          Cancel_handle.cancel ev)
    ) else (
      cb ();
      Cancel_handle.dummy
    )
  in

  { poll; wait }

let ev_timeout f : _ Event.t =
  let deadline = Time.monotonic_time_s () +. f in
  ev_deadline deadline
