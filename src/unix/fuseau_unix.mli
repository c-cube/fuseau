class ev_loop : Event_loop.t
(** A unix-based event loop *)

val main : (unit -> 'a) -> 'a
(** A version of {!Fuseau.main} that uses a Unix-based {!ev_loop}. *)
