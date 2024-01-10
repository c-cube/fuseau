(** Resource pool.

    This pool can be used for buffers. It can be used for other resources
    but do note that it assumes resources are still reasonably
    cheap to produce and discard, and will never block waiting for
    a resource — it's not a good pool for DB connections. *)

type 'a t
(** Pool of values of type ['a] *)

val create :
  ?clear:('a -> unit) -> mk_item:(unit -> 'a) -> ?max_size:int -> unit -> 'a t
(** Create a new pool.
    @param mk_item produce a new item in case the pool is empty
    @param max_size maximum number of item in the pool before we start
      dropping resources on the floor. This controls resource consumption.
    @param clear a function called on items before recycling them.
 *)

val acquire : 'a t -> 'a
val recycle : 'a t -> 'a -> unit

val with_resource : 'a t -> ('a -> 'b) -> 'b
(** [with_resource pool f] runs [f x] with [x] a resource;
    when [f] fails or returns, [x] is returned to the pool for
    future reuse. *)
