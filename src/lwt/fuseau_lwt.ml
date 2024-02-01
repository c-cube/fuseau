include Fuseau

open struct
  let[@inline] conv_handle (ev : Lwt_engine.event) : Cancel_handle.t =
    let cancel () = Lwt_engine.stop_event ev in
    { Cancel_handle.cancel }

  let _pp_pending out engine =
    Printf.fprintf out "readc=%d writec=%d timerc=%d" engine#readable_count
      engine#writable_count engine#timer_count
end

class ev_loop (engine : Lwt_engine.t) : Event_loop.t =
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
      Lwt.wakeup_paused ();
      engine#iter block
    (* Printf.printf "lwt one step done %a\n%!" _pp_pending engine *)
  end

(* TODO:
   - [spawn_as_lwt : (unit -> 'a) -> 'a Lwt.t]
     (creates a lwt/lwt-promise, spawns a fiber that eventually fulfill promise,
       return lwt)

   - [spawn_as_lwt_as_child_of : _ fiber -> (unit -> 'a) -> 'a Lwt.t]
       same but with explicit ownership

   - [wait_fiber : 'a Fiber.t -> 'a Lwt.t]
*)

(** The global loop using {!Lwt_engine.get()} *)
let create () : Event_loop.t =
  let engine : Lwt_engine.t = Lwt_engine.get () in
  new ev_loop engine

let main (f : unit -> 'a) : 'a =
  let loop = create () in
  Fuseau.main ~loop f

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
