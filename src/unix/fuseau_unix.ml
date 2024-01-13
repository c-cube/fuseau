open Common_

type event_handle = Event_loop.event_handle = { cancel: unit -> unit }
[@@unboxed]

type io_mode =
  | Read
  | Write

module IO_wait = struct
  type t = {
    mutable active: bool;
    f: event_handle -> unit;
    as_event_handle: event_handle;
  }
  (** A single event, waiting on a unix FD *)

  let make f : t =
    let rec self =
      {
        active = true;
        f;
        as_event_handle = { cancel = (fun () -> self.active <- false) };
      }
    in
    self
end

module Per_fd = struct
  type t = {
    fd: Unix.file_descr;
    mutable reads: IO_wait.t list;
    mutable writes: IO_wait.t list;
  }

  let[@inline] is_empty self = self.reads = [] && self.writes = []
end

module IO_tbl = struct
  type t = {
    mutable n_read: int;
    mutable n_write: int;
    tbl: (Unix.file_descr, Per_fd.t) Hashtbl.t;
  }

  let create () : t = { tbl = Hashtbl.create 32; n_read = 0; n_write = 0 }

  let get_or_create (self : t) fd : Per_fd.t =
    try Hashtbl.find self.tbl fd
    with Not_found ->
      let per_fd = { Per_fd.fd; reads = []; writes = [] } in
      Hashtbl.add self.tbl fd per_fd;
      per_fd

  let add_io_wait (self : t) fd mode (ev : IO_wait.t) =
    let per_fd = get_or_create self fd in
    match mode with
    | Read ->
      self.n_read <- 1 + self.n_read;
      per_fd.reads <- ev :: per_fd.reads
    | Write ->
      self.n_write <- 1 + self.n_write;
      per_fd.writes <- ev :: per_fd.writes

  let prepare_select (self : t) =
    let reads = ref [] in
    let writes = ref [] in
    Hashtbl.iter
      (fun _ (per_fd : Per_fd.t) ->
        if Per_fd.is_empty per_fd then
          Hashtbl.remove self.tbl per_fd.fd
        else (
          if per_fd.reads <> [] then reads := per_fd.fd :: !reads;
          if per_fd.writes <> [] then writes := per_fd.fd :: !writes
        ))
      self.tbl;
    !reads, !writes

  let trigger_waiter (io : IO_wait.t) =
    if io.active then io.f io.as_event_handle

  let handle_ready (self : t) (reads : Unix.file_descr list)
      (writes : Unix.file_descr list) : unit =
    List.iter
      (fun fd ->
        let per_fd = Hashtbl.find self.tbl fd in
        List.iter trigger_waiter per_fd.reads;
        self.n_read <- self.n_read - List.length per_fd.reads;
        per_fd.reads <- [])
      reads;

    List.iter
      (fun fd ->
        let per_fd = Hashtbl.find self.tbl fd in
        List.iter trigger_waiter per_fd.writes;
        self.n_write <- self.n_write - List.length per_fd.writes;
        per_fd.writes <- [])
      writes;
    ()
end

let run_timer_ (t : Timer.t) =
  let rec loop () =
    match Timer.next t with
    | Timer.Empty -> None
    | Timer.Run (f, ev_h) ->
      f ev_h;
      loop ()
    | Timer.Wait f ->
      if f > 0. then
        Some f
      else
        None
  in
  loop ()

class ev_loop : Event_loop.t =
  let _timer = Timer.create () in
  let _io_wait : IO_tbl.t = IO_tbl.create () in

  object
    (* val read_ : (event_handle -> unit) Int_tbl.t = Int_tbl.create 32 *)
    method one_step ~block () : unit =
      let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "fuseau.unix.one-step" in
      let delay = run_timer_ _timer in

      let delay =
        if block then
          Option.value delay ~default:10.
        else
          (* do not wait *)
          0.
      in

      let reads, writes = IO_tbl.prepare_select _io_wait in
      if reads <> [] || writes <> [] || Timer.has_tasks _timer then (
        Trace.messagef (fun k ->
            k "select %d reads, %d writes, delay=%.3f" (List.length reads)
              (List.length writes) delay);

        let reads, writes, _ = Unix.select reads writes [] delay in
        IO_tbl.handle_ready _io_wait reads writes
      );
      ()

    method on_readable
        : Unix.file_descr -> (event_handle -> unit) -> event_handle =
      fun fd f : event_handle ->
        let ev = IO_wait.make f in
        IO_tbl.add_io_wait _io_wait fd Read ev;
        ev.as_event_handle

    method on_writable
        : Unix.file_descr -> (event_handle -> unit) -> event_handle =
      fun fd f : event_handle ->
        let ev = IO_wait.make f in
        IO_tbl.add_io_wait _io_wait fd Write ev;
        ev.as_event_handle

    method on_timer
        : float -> repeat:bool -> (event_handle -> unit) -> event_handle =
      fun delay ~repeat f ->
        if repeat then
          Timer.run_every _timer delay f
        else
          Timer.run_after _timer delay f

    (* TODO: remove?? *)
    method fake_io : Unix.file_descr -> unit = assert false
    method readable_count : int = _io_wait.n_read
    method writable_count : int = _io_wait.n_write
    method timer_count : int = Timer.num_tasks _timer
  end
