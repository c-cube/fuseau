open Utils_

let sleep_s delay : unit =
  if delay > 50e-9 then (
    let sched = get_sched "sleep" () in
    let loop = Scheduler.Internal_.ev_loop sched in
    Fiber.Internal_.suspend ~before_suspend:(fun ~wakeup ->
        ignore
          (Event_loop.on_timer loop ~repeat:false delay (fun ev ->
               wakeup ();
               Cancel_handle.cancel ev)
            : Cancel_handle.t);
        ())
  )
