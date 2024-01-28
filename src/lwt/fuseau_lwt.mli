val await : 'a Lwt.t -> 'a
(** Like {!Fuseau.await} but on a Lwt promise. *)

val spawn_as_lwt :
  ?name:string -> ?propagate_cancel_to_parent:bool -> (unit -> 'a) -> 'a Lwt.t

val spawn_as_lwt_from_anywhere :
  ?name:string -> Scheduler.t -> (unit -> 'a) -> 'a Lwt.t

val main : (unit -> 'a) -> 'a
(** Run main loop, using the current [Lwt_engine.t]. *)
