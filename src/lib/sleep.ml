open Common_
open Fuseau_core

let sleep delay : unit =
  if delay > 50e-9 then (
    let sched = get_sched "sleep" () in
    let loop = Scheduler.Internal_.ev_loop sched in
    Fiber.Internal_.suspend ~before_suspend:(fun ~wakeup ->
        ignore
          (loop#on_timer ~repeat:false delay (fun _ev -> wakeup ())
            : event_handle);
        ())
  )
