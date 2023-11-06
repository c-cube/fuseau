(** Low level, bidirectional byte streams *)

open Utils_

type 'kind t = 'kind Luv.Stream.t
type any = Any : [ `Stream of _ ] Luv.Stream.t -> any [@@unboxed]

let shutdown (self : _ t) : unit =
  await_cb_ () @@ fun k -> Luv.Stream.shutdown self k

let listen ?backlog (self : _ t) : unit =
  await_cb_ () @@ fun k -> Luv.Stream.listen ?backlog self k

let accept ~server ~client : unit =
  Luv.Stream.accept ~server ~client |> unwrap_luv_res

let[@inline] read_start ?allocate (self : _ t)
    (k : (Buffer.t, Luv.Error.t) result -> unit) : unit =
  Luv.Stream.read_start ?allocate self k

let read_stop (self : _ t) : unit = Luv.Stream.read_stop self |> unwrap_luv_res

let try_write (self : _ t) bufs : int option =
  match Luv.Stream.try_write self bufs with
  | Ok n -> Some n
  | Error _ -> None

let rec write (self : _ t) bufs : unit =
  if bufs = [] then
    ()
  else (
    match try_write self bufs with
    | Some n ->
      let bufs = Buffer.drop bufs n in
      if bufs <> [] then write self bufs
    | None ->
      (* we need to block *)
      let trigger = Trigger.create () in
      let bufs = ref bufs in
      let ebt = ref (Ok ()) in
      Luv.Stream.write self !bufs (fun r n ->
          bufs := Buffer.drop !bufs n;
          ebt := r);
      Trigger.await_or_raise trigger;

      unwrap_luv_res !ebt;
      write self !bufs
  )

(* TODO: write2, read2 *)

let is_readable : _ t -> bool = Luv.Stream.is_readable
let is_writable : _ t -> bool = Luv.Stream.is_writable

let set_blocking (self : _ t) (b : bool) : unit =
  Luv.Stream.set_blocking self b |> Err.unwrap_luv

type connect_request = Luv.Stream.Connect_request.t

let coerce = Luv.Stream.coerce

(** {2 Higher-level API} *)

open struct
  let dummy_luv_err = Error `EOF

  type 'kind state = {
    str: 'kind t;
    mutable closed: bool;
    mutable locked: bool;
  }

  let dispose self =
    if not self.closed then (
      self.closed <- true;
      shutdown self.str
    )

  class base (st : _ state) =
    object
      method dispose = dispose st
      method is_in_use = st.locked
    end

  class base_reader (st : _ state) : Stream.Reader.byob_t =
    object (self_reader)
      method dispose = st.locked <- false
      method closed = st.closed

      method read_into buf i len : int =
        if st.closed then invalid_arg "stream reader: stream is closed";
        let res = ref dummy_luv_err in
        read_start
          ~allocate:(fun _ -> Buffer.sub buf ~offset:i ~length:len)
          st.str
          (fun r ->
            read_stop st.str;
            res := r);
        let buf = Err.unwrap_luv !res in
        Buffer.size buf

      method read : _ option =
        let buf = Buffer.create 64 in
        let n = self_reader#read_into buf 0 64 in
        if n = 0 then
          None
        else
          Some (Buffer.sub buf ~offset:0 ~length:n)
    end

  class base_readable_stream ~allocate (st : _ state) =
    object (self_str)
      method get_byob_reader : Stream.Reader.byob_t =
        if st.closed then invalid_arg "stream is closed";
        if st.locked then raise Stream.Already_in_use;
        st.locked <- true;
        new base_reader st

      method get_reader = (self_str#get_byob_reader :> _ Stream.Reader.t)

      method pipe_into_byte_stream : 'w. (#Stream.Writable.byte_t as 'w) -> unit
          =
        fun w ->
          let reader = self_str#get_byob_reader in
          let writer = w#get_byob_writer in
          let buf = allocate (32 * 1024) in

          let continue = ref true in
          while !continue do
            let n = reader#read_into buf 0 (Buffer.size buf) in
            if n = 0 then
              continue := false
            else
              writer#write_slice buf 0 n
          done
    end

  (** A writer *)
  class base_writer (st : _ state) : Stream.Writer.byob_t =
    object
      method dispose = st.locked <- false
      method closed = st.closed

      method write_slice buf i len : unit =
        if st.closed then invalid_arg "stream writer: stream is closed";
        write st.str [ Buffer.sub buf ~offset:i ~length:len ]

      method writev bufs : unit =
        if st.closed then invalid_arg "stream writer: stream is closed";
        write st.str bufs

      method write buf : unit = write st.str [ buf ]
    end

  class base_writable_stream (st : _ state) =
    object (self_str)
      method get_byob_writer : Stream.Writer.byob_t =
        if st.closed then invalid_arg "stream is closed";
        if st.locked then raise Stream.Already_in_use;
        st.locked <- true;
        new base_writer st

      method get_writer = (self_str#get_byob_writer :> _ Stream.Writer.t)
    end
end

class readable_stream ?(allocate = Buffer.create) (self : _ t) :
  Stream.Readable.byte_t =
  let st = { str = self; closed = false; locked = false } in
  object
    inherit base st
    inherit base_readable_stream ~allocate st
  end

class writable_stream (self : _ t) : Stream.Writable.byte_t =
  let st = { str = self; closed = false; locked = false } in
  object
    inherit base st
    inherit base_writable_stream st
  end

class readable_writable_stream ?(allocate = Buffer.create) (self : _ t) :
  Stream.ReadWritable.byte_t =
  let st = { str = self; closed = false; locked = false } in
  object
    inherit base st
    inherit base_readable_stream st ~allocate
    inherit base_writable_stream st
  end
