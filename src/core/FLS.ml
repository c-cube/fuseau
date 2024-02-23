open Common_
open Types

type 'a key = 'a Types.fls_key

let key_count_ = A.make 0

let new_key (type t) ~init () : t key =
  let offset = A.fetch_and_add key_count_ 1 in
  (module struct
    type nonrec t = t
    type fls_value += V of t

    let offset = offset
    let init = init
  end : FLS_KEY
    with type t = t)

type fls_value += Dummy

(** Resize array of TLS values *)
let[@inline never] resize_ (fib : _ Fiber.t) n =
  let len = Array.length fib.fls in
  let new_fls = Array.make (max n (len * 2)) Dummy in
  Array.blit fib.fls 0 new_fls 0 len;
  fib.fls <- new_fls

(** Access current fiber, or fail *)
let[@inline] cur_fiber_ () : any_fiber =
  match !Fiber.get_current_ () with
  | Some f -> f
  | None -> failwith "FLS: must be run from inside a fiber"

let get (type a) ((module K) : a key) : a =
  let (Any_fiber fib) = cur_fiber_ () in
  if K.offset >= Array.length fib.fls then resize_ fib K.offset;
  match fib.fls.(K.offset) with
  | K.V x -> (* common case first *) x
  | Dummy ->
    (* first time we access this *)
    let v = K.init () in
    fib.fls.(K.offset) <- K.V v;
    v
  | _ -> assert false

let set (type a) ((module K) : a key) (v : a) : unit =
  let (Any_fiber fib) = cur_fiber_ () in
  if K.offset >= Array.length fib.fls then resize_ fib K.offset;
  fib.fls.(K.offset) <- K.V v

let with_value key x f =
  let old = get key in
  set key x;
  Fun.protect ~finally:(fun () -> set key old) f
