open Common_

open struct
  module T = Luv.Timer
end

let sleep_ms (n : int) =
  let loop = Loop.Current.get_exn () in
  let t = T.init ~loop:(Loop.loop loop) () |> Err.unwrap_luv in
  let finally () = T.stop t |> Err.unwrap_luv in
  let@ () = Fun.protect ~finally in
  Suspend.suspend (fun ~wakeup -> T.start t n wakeup |> Err.unwrap_luv)

let every ?(start_ms = 0) ~repeat_ms (f : unit -> unit) : Disposable.t =
  let loop = Loop.Current.get_exn () in
  let t = T.init ~loop:(Loop.loop loop) () |> Err.unwrap_luv in
  T.start t ~repeat:repeat_ms start_ms f |> Err.unwrap_luv;
  { Disposable.dispose = (fun () -> T.stop t |> Err.unwrap_luv) }
