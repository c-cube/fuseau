(** Interoperability between Fuseau and Lwt.

    This combines {!Fuseau}'s fibers with the Lwt event loop
    (ie {!Lwt_engine}) to deal with timers and file descriptors
    readiness. In essence, one can use fibers and structured concurrency
    from [Fuseau] alongside Lwt libraries and Lwt IO operations.
*)

(** {2 Re-export of fuseau} *)

include module type of struct
  include Fuseau
end

(** {2 Interop with Lwt}

    Here we have functions that are used to cross
    the boundary between the Fuseau world
    and the Lwt world. *)

val await_lwt : 'a Lwt.t -> 'a
(** Like {!Fuseau.await} but on a Lwt promise. *)

val spawn_as_lwt :
  ?parent:_ Fiber.t ->
  ?name:string ->
  ?propagate_cancel_to_parent:bool ->
  (unit -> 'a) ->
  'a Lwt.t
(** [spawn_as_lwt f] runs [f()] in the Fuseau+Lwt thread, and returns
    a Lwt future that resolves when the fiber does *)

val spawn_as_lwt_from_anywhere :
  ?name:string -> Scheduler.t -> (unit -> 'a) -> 'a Lwt.t
(** [spawn_from_anywhere scheduler f] runs [f()] as a toplevel fiber
      in [scheduler]. It can be run from another thread. *)

(** IO through Lwt's engine *)
module IO_lwt : sig
  type file_descr = Unix.file_descr

  val read : file_descr -> bytes -> int -> int -> int
  val write_once : file_descr -> bytes -> int -> int -> int
  val write : file_descr -> bytes -> int -> int -> unit
end

val ev_read : Unix.file_descr -> bytes -> int -> int -> int Event.t
(** [ev_read fd buf i len] is an event that, when ready,
    will ready at most [len] bytes from [fd] into [buf[i.. i+len]],
    and return how many bytes were read. It uses [Lwt_engine]
    to wait for [fd]'s readiness. *)

val ev_write : Unix.file_descr -> bytes -> int -> int -> int Event.t
(** [ev_write fd buf i len] is an event that, when ready,
    writes at most [len] bytes from [buf] into [fd]. It uses
    [Lwt_engine] to wait for [fd]'s readiness. *)

(** Iostream specialized for Lwt *)
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

(** Networking using [Lwt_io] *)
module Net : sig
  module TCP_server : sig
    type t = Lwt_io.server
    (** The Lwt TCP server *)

    val establish :
      ?backlog:int ->
      ?no_close:bool ->
      Unix.sockaddr ->
      (Unix.sockaddr -> Iostream.In.t -> Iostream.Out.t -> unit) ->
      t
    (** Establish a server, where each connection spawns a new
        fiber that uses Fuseau's Iostreams. *)

    val shutdown : t -> unit
  end

  module TCP_client : sig
    val with_connect :
      Unix.sockaddr -> (Iostream.In.t -> Iostream.Out.t -> 'a) -> 'a
    (** Use Lwt to connect over TCP *)
  end
end

(** {2 Main loop} *)

val main : (unit -> 'a) -> 'a
(** Run main loop, using the current [Lwt_engine.t]. *)
