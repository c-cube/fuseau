open Common_

open struct
  module T = Luv.Timer
end

let sleep_ms (n : int) =
  let loop = get_cur_loop_exn () in
  let t = T.init ~loop () |> Err.unwrap_luv in
  let finally () = T.stop t |> Err.unwrap_luv in
  let@ () = Fun.protect ~finally in

  Effect.perform
  @@ Effects_.Suspend
       { suspended = (fun ~wakeup -> T.start t n wakeup |> Err.unwrap_luv) }
