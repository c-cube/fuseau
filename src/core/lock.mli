(** Mutex-protected resource. *)

type 'a t
(** A value protected by a mutex *)

val create : 'a -> 'a t
(** Create a new protected value. *)

val mutex : _ t -> Mutex.t

val with_ : 'a t -> ('a -> 'b) -> 'b
(** [with_ l f] runs [f x] where [x] is the value protected with
    the lock [l], in a critical section. If [f x] fails, [with_lock l f]
    fails too but the lock is released. *)

val update : 'a t -> ('a -> 'a) -> unit
(** [update l f] replaces the content [x] of [l] with [f x], while protected
    by the mutex. *)

val update_map : 'a t -> ('a -> 'a * 'b) -> 'b
(** [update_map l f] computes [x', y = f (get l)], then puts [x'] in [l]
    and returns [y], while protected by the mutex. *)

val map : ('a -> 'b) -> 'a t -> 'b

val map_no_exn : ('a -> 'b) -> 'a t -> 'b
(** Like {!map} but the function cannot raise.
    If the function raise the behavior is unspecified. *)
