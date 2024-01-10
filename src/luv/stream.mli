type 'a t

exception Closed

type awakener = unit -> unit

type 'a writer = {
  try_push: 'a -> bool;
  call_to_resume: awakener -> unit;
}

type buf_writer = Luv.Buffer.t writer
type buf_t = Luv.Buffer.t t

val create : ?max_size:int -> ?compute_size:('a -> int) -> unit -> 'a t
val create_byte : ?max_size:int -> unit -> buf_t
val get_writer : 'a t -> 'a writer
val close : _ t -> unit
val try_push : 'a t -> 'a -> bool
val push : 'a t -> 'a -> unit
val pop : 'a t -> 'a option
val is_empty : _ t -> bool
val is_full : _ t -> bool
val size : _ t -> int
