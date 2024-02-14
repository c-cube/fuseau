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

(** IO through Lwt's engine *)
module IO_lwt : sig
  type file_descr = Unix.file_descr

  val read : file_descr -> bytes -> int -> int -> int
  val write_once : file_descr -> bytes -> int -> int -> int
  val write : file_descr -> bytes -> int -> int -> unit
end

val ev_read : Unix.file_descr -> bytes -> int -> int -> int Event.t
val ev_write : Unix.file_descr -> bytes -> int -> int -> int Event.t

module Iostream : sig
  module In : sig
    include module type of Iostream.In

    val of_unix_fd : ?close_noerr:bool -> ?buf:bytes -> Unix.file_descr -> t
  end

  module Out : sig
    include module type of Iostream.Out

    val of_unix_fd : ?close_noerr:bool -> ?buf:bytes -> Unix.file_descr -> t
  end
end

module Net : sig
  module TCP_server : sig
    type t = Lwt_io.server

    val establish :
      ?backlog:int ->
      ?no_close:bool ->
      Unix.sockaddr ->
      (Unix.sockaddr -> Iostream.In.t -> Iostream.Out.t -> unit) ->
      t

    val shutdown : t -> unit
  end

  module TCP_client : sig
    val with_connect :
      Unix.sockaddr -> (Iostream.In.t -> Iostream.Out.t -> 'a) -> 'a
  end
end

(** {2 Main loop} *)

val main : (unit -> 'a) -> 'a
(** Run main loop, using the current [Lwt_engine.t]. *)
