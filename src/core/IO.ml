open Utils_

let rec read fd buf i len : int =
  if len = 0 then
    0
  else (
    match Unix.read fd buf i len with
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      let sched = get_sched "read" () in
      let loop = Scheduler.Internal_.ev_loop sched in
      (* wait for FD to be ready *)
      Fiber.Internal_.suspend ~before_suspend:(fun ~wakeup ->
          ignore (loop#on_readable fd (fun _ev -> wakeup ()) : Cancel_handle.t));
      read fd buf i len
    | n -> n
  )

let rec write_once fd buf i len : int =
  if len = 0 then
    0
  else (
    match Unix.write fd buf i len with
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      let sched = get_sched "write_once" () in
      let loop = Scheduler.Internal_.ev_loop sched in
      (* wait for FD to be ready *)
      Fiber.Internal_.suspend ~before_suspend:(fun ~wakeup ->
          ignore (loop#on_writable fd (fun _ev -> wakeup ()) : Cancel_handle.t));
      write_once fd buf i len
    | n -> n
  )

let write fd buf i len : unit =
  let i = ref i in
  let len = ref len in
  while !len > 0 do
    let n = write_once fd buf !i !len in
    i := !i + n;
    len := !len - n
  done
