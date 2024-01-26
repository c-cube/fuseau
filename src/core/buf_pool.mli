(** A pool of buffers to reuse. *)

type t
(** Buffer pool.

    This type is thread-safe. *)

val create : ?buf_size:int -> ?max_size:int -> unit -> t
(** Create a pool *)

val acquire : t -> bytes
(** Take a buffer from the pool. Once done with it, the buffer
    should be {!recycle}'d. *)

val recycle : t -> bytes -> unit
(** Give a buffer back to the pool. *)

val with_buf : t -> (bytes -> 'a) -> 'a
