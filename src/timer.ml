open Common_

open struct
  module T = Luv.Timer
end

let sleep_ms (n : int) =
  let loop = Event_loop.Private.get_current_exn () in
  let t = T.init ~loop:(Event_loop.loop loop) () |> Err.unwrap_luv in
  let finally () = T.stop t |> Err.unwrap_luv in
  let@ () = Fun.protect ~finally in

  let trigger = Trigger.create () in
  T.start t n (fun () -> Trigger.signal trigger) |> Err.unwrap_luv;
  Trigger.await_or_raise trigger

let every ?(start_ms = 0) ~repeat_ms (f : unit -> unit) : Disposable.t =
  let loop = Event_loop.Private.get_current_exn () in
  let t = T.init ~loop:(Event_loop.loop loop) () |> Err.unwrap_luv in
  T.start t ~repeat:repeat_ms start_ms f |> Err.unwrap_luv;
  { Disposable.dispose = (fun () -> T.stop t |> Err.unwrap_luv) }
