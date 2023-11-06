let ( let@ ) = ( @@ )

let get_cur_luv_loop () =
  Event_loop.Private.get_current_exn () |> Event_loop.loop

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

let await_cb_noinit_ (f : (('a, Luv.Error.t) result -> unit) -> unit) : 'a =
  let trigger = Trigger.create () in
  let ebt = ref (Ok None) in
  f (fun res ->
      Trigger.signal trigger;
      match res with
      | Ok x -> ebt := Ok (Some x)
      | Error _ as e -> ebt := e);
  Trigger.await_or_raise trigger;
  match unwrap_luv_res !ebt with
  | Some x -> x
  | None -> assert false
