open struct
  let[@inline] conv_handle (ev : Lwt_engine.event) : Cancel_handle.t =
    let cancel () = Lwt_engine.stop_event ev in
    { Cancel_handle.cancel }

  (*let pp_pending out engine =
    Printf.fprintf out "readc=%d writec=%d timerc=%d" engine#readable_count
      engine#writable_count engine#timer_count*)
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
      Lwt.wakeup_paused ();
      engine#iter block
  end

(** The global loop using {!Lwt_engine.get()} *)
let create () : Event_loop.t =
  let engine : Lwt_engine.t = Lwt_engine.get () in
  new ev_loop engine

let main (f : unit -> 'a) : 'a =
  let loop = create () in
  Fuseau.main ~loop f
