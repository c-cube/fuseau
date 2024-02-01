include Fuseau

let ( let@ ) = ( @@ )

open struct
  let[@inline] conv_handle (ev : Lwt_engine.event) : Cancel_handle.t =
    let cancel () = Lwt_engine.stop_event ev in
    { Cancel_handle.cancel }

  let _pp_pending out engine =
    Printf.fprintf out "readc=%d writec=%d timerc=%d" engine#readable_count
      engine#writable_count engine#timer_count
end

class ev_loop (engine : Lwt_engine.t) : Event_loop.t =
  let _in_blocking_section = ref false in
  let _promise = ref (snd @@ Lwt.wait ()) in
  object
    method fake_io fd = engine#fake_io fd

    method has_pending_tasks =
      engine#readable_count > 0 || engine#writable_count > 0
      || engine#timer_count > 0

    method on_readable fd f : Cancel_handle.t =
      engine#on_readable fd (fun ev -> f (conv_handle ev)) |> conv_handle

    method on_writable fd f : Cancel_handle.t =
      engine#on_writable fd (fun ev -> f (conv_handle ev)) |> conv_handle

    method on_timer time ~repeat f =
      engine#on_timer time repeat (fun ev -> f (conv_handle ev)) |> conv_handle

    method one_step ~block () =
      (* Printf.printf "lwt one step block=%b %a\n%!" block _pp_pending engine; *)
      let@ _sp =
        Trace_core.with_span ~__FILE__ ~__LINE__ "fuseau-lwt.one-step"
          ~data:(fun () -> [ "block", `Bool block ])
      in
      Lwt.wakeup_paused ();
      _in_blocking_section := true;
      engine#iter block;
      _in_blocking_section := false
    (* Printf.printf "lwt one step done %a\n%!" _pp_pending engine *)

    method interrupt_if_in_blocking_section =
      if !_in_blocking_section then (
        (* immediate wakeup *)
        (* Printf.eprintf "WAKEUP LWT\n%!"; *)
        (* _in_blocking_section := false; *)
        Lwt.wakeup !_promise ();
        _promise := snd @@ Lwt.wait ();
        ()
      )
  end

let main (f : unit -> 'a) : 'a =
  let engine : Lwt_engine.t = Lwt_engine.get () in
  let loop = new ev_loop engine in
  try
    let x = Fuseau.main ~loop f in
    engine#destroy;
    x
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    engine#destroy;
    Printexc.raise_with_backtrace e bt

let await_lwt (fut : _ Lwt.t) =
  match Lwt.poll fut with
  | Some x -> x
  | None ->
    (* suspend fiber, wake it up when [fut] resolves *)
    Fuseau.Private_.suspend ~before_suspend:(fun ~wakeup ->
        Lwt.on_termination fut wakeup);

    (match Lwt.poll fut with
    | Some x -> x
    | None -> assert false)

let spawn_as_lwt ?name ?propagate_cancel_to_parent (f : unit -> 'a) : 'a Lwt.t =
  let fut, promise = Lwt.wait () in
  let _fib =
    Fuseau.spawn ?name ?propagate_cancel_to_parent (fun () ->
        try
          let x = f () in
          Lwt.wakeup promise x
        with exn -> Lwt.wakeup_exn promise exn)
  in
  fut

let spawn_as_lwt_from_anywhere ?name sched f : 'a Lwt.t =
  let fut, promise = Lwt.wait () in
  let _fib =
    Fuseau.spawn_from_anywhere ?name sched (fun () ->
        try
          let x = f () in
          Lwt.wakeup promise x
        with exn -> Lwt.wakeup_exn promise exn)
  in
  fut
