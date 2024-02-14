type 'a t = {
  poll: unit -> 'a Exn_bt.result option;
  wait: (unit -> unit) -> Cancel_handle.t;
}

type 'ret branch = When : 'a t * ('a -> 'ret) -> 'ret branch

let rec select_rec_ brs =
  let rec poll_list_ = function
    | When (ev, f) :: tl ->
      (* see if [ev] is ready *)
      (match ev.poll () with
      | Some (Ok x) -> (f [@tailcall]) x
      | Some (Error ebt) -> Exn_bt.raise ebt
      | None -> poll_list_ tl)
    | [] ->
      (* no branch worked. Now let's wait. *)
      let cancel_handlers = ref [] in
      Fiber.suspend ~before_suspend:(fun ~wakeup ->
          List.iter
            (fun (When (ev, _)) ->
              let cancel = ev.wait wakeup in
              if cancel != Cancel_handle.dummy then
                cancel_handlers := cancel :: !cancel_handlers)
            brs);

      List.iter (fun cb -> Cancel_handle.cancel cb) !cancel_handlers;

      (* poll again *)
      (select_rec_ [@tailcall]) brs
  in

  poll_list_ brs

let[@inline] select (brs : _ branch list) =
  match brs with
  | [] -> assert false
  | brs -> select_rec_ brs
