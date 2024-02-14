type waker = unit -> unit

let[@inline] suspend ~before_suspend =
  Effect.perform @@ Effects.Suspend { before_suspend }

type 'a t = {
  max_size: int;
  q: 'a Queue.t;
  receivers: waker Queue.t;
  senders: waker Queue.t;
  mutable closed: bool;
}

let create ?(max_size = max_int) () : _ t =
  {
    max_size;
    q = Queue.create ();
    receivers = Queue.create ();
    senders = Queue.create ();
    closed = false;
  }

exception Closed

let[@inline] size self = Queue.length self.q
let[@inline] is_empty self = Queue.is_empty self.q
let[@inline] is_full_ self : bool = Queue.length self.q >= self.max_size

let close self : unit =
  self.closed <- true;
  (* wakeup everyone *)
  Queue.iter (fun f -> f ()) self.senders;
  Queue.iter (fun f -> f ()) self.receivers;
  ()

let wakeup_receivers self =
  while not (Queue.is_empty self.receivers) do
    let r = Queue.pop self.receivers in
    r ()
  done

let send self x : unit =
  let continue = ref true in
  while !continue do
    if self.closed then raise Closed;

    if is_full_ self then
      suspend ~before_suspend:(fun ~wakeup -> Queue.push wakeup self.senders)
    else (
      continue := false;
      Queue.push x self.q;
      wakeup_receivers self
    )
  done

let try_send self x : bool =
  if self.closed then raise Closed;
  if is_full_ self then
    false
  else (
    Queue.push x self.q;
    wakeup_receivers self;
    true
  )

let on_send_ready self cb =
  if is_full_ self then
    Queue.push cb self.senders
  else
    cb ()

let wakeup_senders self =
  while not (Queue.is_empty self.senders) do
    let w = Queue.pop self.senders in
    w ()
  done

let rec receive_exn (self : 'a t) : 'a =
  match Queue.pop self.q with
  | x ->
    wakeup_senders self;
    x
  | exception Queue.Empty ->
    if self.closed then raise Closed;

    suspend ~before_suspend:(fun ~wakeup -> Queue.push wakeup self.receivers);
    receive_exn self

let try_receive (self : 'a t) : 'a option =
  if Queue.is_empty self.q then (
    if self.closed then raise Closed;
    None
  ) else (
    let x = Queue.pop self.q in
    wakeup_senders self;
    Some x
  )

let on_receive_ready (self : _ t) cb : unit =
  if Queue.is_empty self.q then
    Queue.push cb self.receivers
  else
    cb ()

let ev_send c x : unit Event.t =
  let poll () =
    if try_send c x then
      Some (Ok ())
    else
      None
  in
  let wait cb =
    on_send_ready c cb;
    Cancel_handle.dummy
  in
  { poll; wait }

let ev_receive (self : 'a t) : 'a Event.t =
  let poll () =
    match try_receive self with
    | Some x -> Some (Ok x)
    | None -> None
    | exception (Closed as exn) ->
      let ebt = Exn_bt.get exn in
      Some (Error ebt)
  in
  let wait cb =
    on_receive_ready self cb;
    Cancel_handle.dummy
  in
  { poll; wait }
