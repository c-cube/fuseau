(** Interoperability between Fuseau and Lwt *)

(** {2 Re-export of fuseau} *)

include module type of struct
  include Fuseau
end

(** {2 Interop *)

val await_lwt : 'a Lwt.t -> 'a
(** Like {!Fuseau.await} but on a Lwt promise. *)

val spawn_as_lwt :
  ?name:string -> ?propagate_cancel_to_parent:bool -> (unit -> 'a) -> 'a Lwt.t

val spawn_as_lwt_from_anywhere :
  ?name:string -> Scheduler.t -> (unit -> 'a) -> 'a Lwt.t

module IO_lwt : sig
  type file_descr = Unix.file_descr

  val read : file_descr -> bytes -> int -> int -> int
  val write_once : file_descr -> bytes -> int -> int -> int
  val write : file_descr -> bytes -> int -> int -> unit
end

module IO_in_lwt : sig
  include module type of IO_in

  val of_unix_fd : ?close_noerr:bool -> ?buf:bytes -> Unix.file_descr -> t
end

module IO_out_lwt : sig
  include module type of IO_out

  val of_unix_fd : ?close_noerr:bool -> ?buf:bytes -> Unix.file_descr -> t
end

(** {2 Main loop} *)

val main : (unit -> 'a) -> 'a
(** Run main loop, using the current [Lwt_engine.t]. *)
