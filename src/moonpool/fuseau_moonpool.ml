module Fut = Moonpool.Fut

let await_fut (fut : _ Fut.t) =
  match Fut.peek fut with
  | Some (Ok x) -> x
  | Some (Error (e, bt)) -> Printexc.raise_with_backtrace e bt
  | None ->
    (* wait until the future is done *)
    Fuseau.Private_.suspend ~before_suspend:(fun ~wakeup ->
        Fut.on_result fut (fun _ -> wakeup ()));
    Fut.get_or_fail_exn fut
