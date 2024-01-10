type 'a t = {
  mutex: Mutex.t;
  mutable content: 'a;
}

let create content : _ t = { mutex = Mutex.create (); content }

let[@inline] with_ (self : _ t) f =
  Mutex.lock self.mutex;
  try
    let x = f self.content in
    Mutex.unlock self.mutex;
    x
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    Mutex.unlock self.mutex;
    Printexc.raise_with_backtrace exn bt

let[@inline] mutex self = self.mutex
let[@inline] update self f = with_ self (fun x -> self.content <- f x)

let[@inline] update_map l f =
  with_ l (fun x ->
      let x', y = f x in
      l.content <- x';
      y)

let[@inline] map f self = with_ self f

let[@inline] map_no_exn f self =
  Mutex.lock self.mutex;
  match f self.content with
  | x ->
    Mutex.unlock self.mutex;
    x
  | exception _ -> assert false
