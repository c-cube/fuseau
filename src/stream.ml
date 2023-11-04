(** Bidirectional byte streams *)

type 'kind t = 'kind Luv.Stream.t
type any = Stream : [ `Stream of _ ] Luv.Stream.t -> any [@@unboxed]

open struct
  let unwrap_luv_res = function
    | Ok x -> x
    | Error ler ->
      let bt = Printexc.get_callstack 10 in
      Exn_bt.raise { Exn_bt.exn = Err.(E (E_luv ler)); bt }

  let await_cb_ (init : 'a) (f : (('a, Luv.Error.t) result -> unit) -> unit) :
      'a =
    let trigger = Trigger.create () in
    let ebt = ref (Ok init) in
    f (fun res ->
        Trigger.signal trigger;
        ebt := res);
    Trigger.await_or_raise trigger;
    unwrap_luv_res !ebt
end

let shutdown (self : _ t) : unit =
  await_cb_ () @@ fun k -> Luv.Stream.shutdown self k

let listen ?backlog (self : _ t) : unit =
  await_cb_ () @@ fun k -> Luv.Stream.listen ?backlog self k

let accept ~server ~client : unit =
  Luv.Stream.accept ~server ~client |> unwrap_luv_res

let dummy_buf_ = Buffer.create 0

let read_start ?allocate (self : _ t) : Buffer.t =
  await_cb_ Buffer.Private.dummy @@ fun k ->
  Luv.Stream.read_start ?allocate self k

let read_stop (self : _ t) : unit = Luv.Stream.read_stop self |> unwrap_luv_res

(* TODO: basic read *)

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
