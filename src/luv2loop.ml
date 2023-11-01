open Common_
module Fiber = Fiber
module Err = Err
module Timer = Timer

let create_loop () : Luv.Loop.t = Luv.Loop.init () |> Err.unwrap_luv

let on_uncaught_exn =
  ref (fun e bt ->
      Printf.eprintf "luv2loop: uncaught exception:\n%s\n%s\n"
        (Printexc.to_string e)
        (Printexc.raw_backtrace_to_string bt))

let run ?(loop = Luv.Loop.default ()) (main : unit -> 'a) : 'a =
  let@ () = with_cur_loop loop in
  let fut = Run_.run_in_idle_fut ~loop main in
  let did_not_complete : bool = Luv.Loop.run ~loop () in
  ignore did_not_complete;
  Fut.get_or_fail_exn fut
