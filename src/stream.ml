type awakener = unit -> unit

type 'a writer = {
  try_push: 'a -> bool;
  call_to_resume: awakener -> unit;
}

type 'a t = {
  buffered: 'a Queue.t;
  max_size: int;
  mutable cur_size: int;
  readers: [ `Signal ] Picos.Trigger.t Queue.t;
  writers: awakener Queue.t;
  mutable closed: bool;
  compute_size: 'a -> int;
}

type buf_writer = Luv.Buffer.t writer
type buf_t = Luv.Buffer.t t

exception Closed

let close self =
  if not self.closed then (
    self.closed <- true;
    (* wake up everyone *)
    Queue.iter (fun f -> f ()) self.writers;
    Queue.clear self.writers;
    Queue.iter (fun tr -> Picos.Trigger.signal tr) self.readers;
    Queue.clear self.readers
  )

let create ?(max_size = 5) ?(compute_size = fun _ -> 1) () : _ t =
  {
    max_size;
    compute_size;
    buffered = Queue.create ();
    cur_size = 0;
    closed = false;
    readers = Queue.create ();
    writers = Queue.create ();
  }

let create_byte ?(max_size = 64 * 1024) () : buf_t =
  create ~max_size ~compute_size:Luv.Buffer.size ()

let[@inline] is_empty self = self.cur_size = 0
let[@inline] is_full self = self.cur_size >= self.max_size
let[@inline] size self = self.cur_size

let try_push (self : 'a t) (x : 'a) : bool =
  if self.closed then
    raise Closed
  else if self.cur_size < self.max_size then (
    Queue.push x self.buffered;
    self.cur_size <- self.cur_size + self.compute_size x;

    (* maybe awake a reader *)
    (match Queue.pop self.readers with
    | exception Queue.Empty -> ()
    | r -> Picos.Trigger.signal r);

    true
  ) else
    false

let rec push (self : 'a t) (x : 'a) : unit =
  if not (try_push self x) then (
    (* have to wait for some space to free up *)
    let tr = Picos.Trigger.create () in
    Queue.push (fun () -> Picos.Trigger.signal tr) self.writers;
    (match Picos.Trigger.await tr with
    | None -> ()
    | Some ebt -> Exn_bt.raise ebt);
    push self x
  )

let get_writer (self : 'a t) : 'a writer =
  {
    try_push = try_push self;
    call_to_resume = (fun (wake : awakener) -> Queue.push wake self.writers);
  }

let rec pop (self : 'a t) : 'a option =
  match Queue.pop self.buffered with
  | item -> Some item
  | exception Queue.Empty ->
    if self.closed then
      None
    else (
      let tr = Picos.Trigger.create () in
      Queue.push (tr :> [ `Signal ] Picos.Trigger.t) self.readers;
      match Picos.Trigger.await tr with
      | Some ebt -> Exn_bt.raise ebt
      | None -> (pop [@tailcall]) self
    )
