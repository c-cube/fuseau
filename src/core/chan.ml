type waker = unit -> unit

let[@inline] suspend ~before_suspend =
  Effect.perform @@ Effects.Suspend { before_suspend }

type 'a t = {
  max_size: int;
  q: 'a Queue.t;
  readers: waker Queue.t;
  writers: waker Queue.t;
  mutable closed: bool;
}

let create ?(max_size = max_int) () : _ t =
  {
    max_size;
    q = Queue.create ();
    readers = Queue.create ();
    writers = Queue.create ();
    closed = false;
  }

exception Closed

let[@inline] size self = Queue.length self.q
let[@inline] is_empty self = Queue.is_empty self.q
let[@inline] is_full_ self : bool = Queue.length self.q >= self.max_size

let close self : unit =
  self.closed <- true;
  (* wakeup everyone *)
  Queue.iter (fun f -> f ()) self.writers;
  Queue.iter (fun f -> f ()) self.readers;
  ()

let send self x : unit =
  let continue = ref true in
  while !continue do
    if self.closed then raise Closed;

    if is_full_ self then
      suspend ~before_suspend:(fun ~wakeup -> Queue.push wakeup self.writers)
    else (
      continue := false;
      Queue.push x self.q;

      (* wakeup readers if needed *)
      while not (Queue.is_empty self.readers) do
        let r = Queue.pop self.readers in
        r ()
      done
    )
  done

let rec receive_exn (self : 'a t) : 'a =
  match Queue.pop self.q with
  | x ->
    (* wakeup a writer if needed *)
    while not (Queue.is_empty self.writers) do
      let w = Queue.pop self.writers in
      w ()
    done;

    x
  | exception Queue.Empty ->
    if self.closed then raise Closed;

    suspend ~before_suspend:(fun ~wakeup -> Queue.push wakeup self.readers);
    receive_exn self
