open Fuseau_core
module A = Atomic
module TLS = Thread_local_storage

let spf = Printf.sprintf
let ( let@ ) = ( @@ )

type event_handle = Event_loop.event_handle

let[@inline] get_sched what () : Scheduler.t =
  match !(TLS.get Scheduler.Internal_.k_current_scheduler) with
  | None -> failwith @@ spf "%s must run from inside the fuseau scheduler" what
  | Some s -> s
