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

(** Action scheduled from outside the loop *)
module Action = struct
  type event = Lwt_engine.event
  type cb = event -> unit

  (** Action that we ask the lwt loop to perform, from the outside *)
  type t =
    | Wakeup_loop  (** The only point of this is to wakeup the event loop *)
    | Wait_readable of Unix.file_descr * Lwt_engine.event option ref * cb
    | Wait_writable of Unix.file_descr * Lwt_engine.event option ref * cb

  (** Perform the action from within the Lwt thread *)
  let perform (self : t) : unit =
    match self with
    | Wakeup_loop -> ()
    | Wait_readable (fd, r, cb) -> r := Some (Lwt_engine.on_readable fd cb)
    | Wait_writable (fd, r, cb) -> r := Some (Lwt_engine.on_writable fd cb)
end

module Action_queue = struct
  type t = { q: Action.t list Atomic.t } [@@unboxed]

  let create () : t = { q = Atomic.make [] }
  let pop_all (self : t) : _ list = Atomic.exchange self.q []

  (** Push the action and return whether the queue was previously empty *)
  let push (self : t) (a : Action.t) : bool =
    let is_first = ref true in
    while
      let old = Atomic.get self.q in
      if Atomic.compare_and_set self.q old (a :: old) then (
        is_first := old = [];
        false
      ) else
        true
    do
      ()
    done;
    !is_first
end

module Perform_action_in_lwt = struct
  open struct
    let actions_ : Action_queue.t = Action_queue.create ()

    (** Gets the current set of notifications and perform them from inside the
    Lwt thread *)
    let perform_pending_actions () : unit =
      let l = Action_queue.pop_all actions_ in
      List.iter Action.perform l

    let notification : int =
      Lwt_unix.make_notification ~once:false perform_pending_actions
  end

  let schedule (a : Action.t) : unit =
    let is_first = Action_queue.push actions_ a in
    if is_first then Lwt_unix.send_notification notification
end

let _in_blocking_section = Atomic.make false

let ev_loop : Event_loop.t =
  object
    method on_timer time ~repeat f =
      (Lwt_engine.get ())#on_timer time repeat (fun ev -> f (conv_handle ev))
      |> conv_handle

    method one_step ~block () =
      (* Printf.printf "lwt one step block=%b %a\n%!" block _pp_pending engine; *)
      let@ _sp =
        Trace_core.with_span ~__FILE__ ~__LINE__ "fuseau-lwt.one-step"
          ~data:(fun () -> [ "block", `Bool block ])
      in
      Lwt.wakeup_paused ();
      Atomic.set _in_blocking_section true;
      (Lwt_engine.get ())#iter block;
      Atomic.set _in_blocking_section false
    (* Printf.printf "lwt one step done %a\n%!" _pp_pending engine *)

    method interrupt_if_in_blocking_section =
      if Atomic.get _in_blocking_section then
        Perform_action_in_lwt.schedule Action.Wakeup_loop
  end

let main (f : unit -> 'a) : 'a =
  let engine : Lwt_engine.t = Lwt_engine.get () in
  let loop = ev_loop in
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

let spawn_as_lwt ?parent ?name ?propagate_cancel_to_parent (f : unit -> 'a) :
    'a Lwt.t =
  let fut, promise = Lwt.wait () in
  let run () =
    try
      let x = f () in
      Lwt.wakeup promise x
    with exn -> Lwt.wakeup_exn promise exn
  in
  let _fib =
    match parent with
    | None -> Fuseau.spawn ?name ?propagate_cancel_to_parent run
    | Some p ->
      let scheduler = Fuseau.Scheduler.get_for_current_thread () in
      Fuseau.spawn_as_child_of ?name ?propagate_cancel_to_parent scheduler p run
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

open struct
  let _default_buf_size = 16 * 1024
end

module IO_lwt = struct
  type file_descr = Unix.file_descr

  let rec read fd buf i len : int =
    if len = 0 then
      0
    else (
      match Unix.read fd buf i len with
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
        (* wait for FD to be ready *)
        Fuseau.Private_.suspend ~before_suspend:(fun ~wakeup ->
            Perform_action_in_lwt.schedule
            @@ Action.Wait_readable
                 ( fd,
                   ref None,
                   fun _ev ->
                     wakeup ();
                     Lwt_engine.stop_event _ev ));
        read fd buf i len
      | exception Unix.Unix_error (Unix.ECONNRESET, _, _) -> 0
      | n -> n
    )

  let rec write_once fd buf i len : int =
    if len = 0 then
      0
    else (
      match Unix.write fd buf i len with
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
        (* wait for FD to be ready *)
        Fuseau.Private_.suspend ~before_suspend:(fun ~wakeup ->
            Perform_action_in_lwt.schedule
            @@ Action.Wait_writable
                 ( fd,
                   ref None,
                   fun ev ->
                     wakeup ();
                     Lwt_engine.stop_event ev ));
        write_once fd buf i len
      | n -> n
    )

  let write fd buf i len : unit =
    let i = ref i in
    let len = ref len in
    while !len > 0 do
      let n = write_once fd buf !i !len in
      i := !i + n;
      len := !len - n
    done
end

let ev_read fd buf i len : int Event.t =
  let poll () =
    if len = 0 then
      Some (Ok 0)
    else (
      match Unix.read fd buf i len with
      | n -> Some (Ok n)
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
        None
      | exception Unix.Unix_error (Unix.ECONNRESET, _, _) -> Some (Ok 0)
      | exception e ->
        let ebt = Exn_bt.get e in
        Some (Error ebt)
    )
  in
  let wait cb =
    let r = ref None in
    let cancel =
      Cancel_handle.make
        ~cancel:(fun () -> Option.iter Lwt_engine.stop_event !r)
        ()
    in
    Perform_action_in_lwt.schedule
    @@ Action.Wait_readable
         ( fd,
           r,
           fun _ev ->
             cb ();
             Lwt_engine.stop_event _ev );
    cancel
  in
  { poll; wait }

let ev_write fd buf i len : int Event.t =
  let poll () =
    if len = 0 then
      Some (Ok 0)
    else (
      match Unix.write fd buf i len with
      | n -> Some (Ok n)
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
        None
      | exception Unix.Unix_error (Unix.ECONNRESET, _, _) -> Some (Ok 0)
      | exception e ->
        let ebt = Exn_bt.get e in
        Some (Error ebt)
    )
  in
  let wait cb =
    let r = ref None in
    let cancel =
      Cancel_handle.make
        ~cancel:(fun () -> Option.iter Lwt_engine.stop_event !r)
        ()
    in
    Perform_action_in_lwt.schedule
    @@ Action.Wait_writable
         ( fd,
           r,
           fun _ev ->
             cb ();
             Lwt_engine.stop_event _ev );
    cancel
  in
  { poll; wait }

module Iostream = struct
  module Out = struct
    include Iostream.Out

    let of_unix_fd ?(close_noerr = false)
        ?(buf = Bytes.create _default_buf_size) fd : t =
      let buf_off = ref 0 in

      let[@inline] is_full () = !buf_off = Bytes.length buf in

      let flush () =
        if !buf_off > 0 then (
          IO_lwt.write fd buf 0 !buf_off;
          buf_off := 0
        )
      in

      object
        method output_char c =
          if is_full () then flush ();
          Bytes.set buf !buf_off c;
          incr buf_off

        method output bs i len : unit =
          let i = ref i in
          let len = ref len in

          while !len > 0 do
            (* make space *)
            if is_full () then flush ();

            let n = min !len (Bytes.length buf - !buf_off) in
            Bytes.blit bs !i buf !buf_off n;
            buf_off := !buf_off + n;
            i := !i + n;
            len := !len - n
          done;
          (* if full, write eagerly *)
          if is_full () then flush ()

        method close () =
          if close_noerr then (
            try
              flush ();
              Unix.close fd
            with _ -> ()
          ) else (
            flush ();
            Unix.close fd
          )

        method flush = flush
      end
  end

  module In = struct
    include Iostream.In

    let of_unix_fd ?(close_noerr = false)
        ?(buf = Bytes.create _default_buf_size) (fd : Unix.file_descr) : t =
      let buf_len = ref 0 in
      let buf_off = ref 0 in

      let refill () =
        buf_off := 0;
        buf_len := IO_lwt.read fd buf 0 (Bytes.length buf)
      in

      object
        method input b i len : int =
          if !buf_len = 0 then refill ();
          let n = min len !buf_len in
          if n > 0 then (
            Bytes.blit buf !buf_off b i n;
            buf_off := !buf_off + n;
            buf_len := !buf_len - n
          );
          n

        method close () =
          if close_noerr then (
            try Unix.close fd with _ -> ()
          ) else
            Unix.close fd
      end
  end
end

module Net = struct
  module TCP_server = struct
    type t = Lwt_io.server

    let establish ?backlog ?no_close addr handler : t =
      let (Any_fiber parent_fiber) = Fuseau.Fiber.get_current () in
      let server =
        Lwt_io.establish_server_with_client_socket ?backlog ?no_close addr
          (fun client_addr client_sock ->
            let ic =
              Iostream.In.of_unix_fd @@ Lwt_unix.unix_file_descr client_sock
            in
            let oc =
              Iostream.Out.of_unix_fd @@ Lwt_unix.unix_file_descr client_sock
            in

            spawn_as_lwt ~parent:parent_fiber ~name:"tcp.server.handler"
              (fun () -> handler client_addr ic oc))
      in
      await_lwt server

    let shutdown self = await_lwt @@ Lwt_io.shutdown_server self
  end

  module TCP_client = struct
    let with_connect addr (f : Iostream.In.t -> Iostream.Out.t -> 'a) : 'a =
      let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.set_nonblock sock;
      Unix.setsockopt sock Unix.TCP_NODELAY true;

      (* connect asynchronously *)
      while
        try
          Unix.connect sock addr;
          false
        with
        | Unix.Unix_error
            ((Unix.EWOULDBLOCK | Unix.EINPROGRESS | Unix.EAGAIN), _, _)
        ->
          Fuseau.Private_.suspend ~before_suspend:(fun ~wakeup ->
              Perform_action_in_lwt.schedule
              @@ Action.Wait_writable
                   ( sock,
                     ref None,
                     fun ev ->
                       wakeup ();
                       Lwt_engine.stop_event ev ));
          true
      do
        ()
      done;

      let ic = Iostream.In.of_unix_fd sock in
      let oc = Iostream.Out.of_unix_fd sock in

      let finally () = try Unix.close sock with _ -> () in
      let@ () = Fun.protect ~finally in
      f ic oc
  end
end
