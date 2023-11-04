let unwrap_luv_res = function
  | Ok x -> x
  | Error ler ->
    let bt = Printexc.get_callstack 10 in
    Exn_bt.raise { Exn_bt.exn = Err.(E (E_luv ler)); bt }

let await_cb_ (init : 'a) (f : (('a, Luv.Error.t) result -> unit) -> unit) : 'a
    =
  let trigger = Trigger.create () in
  let ebt = ref (Ok init) in
  f (fun res ->
      Trigger.signal trigger;
      ebt := res);
  Trigger.await_or_raise trigger;
  unwrap_luv_res !ebt
