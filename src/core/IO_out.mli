(** Output stream. *)

(** An output stream, ie. a place into which we can write bytes.

    This can be a [Buffer.t], an [out_channel], a [Unix.file_descr], etc. *)
class type t =
  object
    method output_char : char -> unit
    (** Output a single char *)

    method output : bytes -> int -> int -> unit
    (** Output slice *)

    method flush : unit -> unit
    (** Flush underlying buffer *)

    method close : unit -> unit
    (** Close the output. Must be idempotent. *)
  end

val create :
  ?flush:(unit -> unit) ->
  ?close:(unit -> unit) ->
  output_char:(char -> unit) ->
  output:(bytes -> int -> int -> unit) ->
  unit ->
  t
(** Create a new output stream from raw components. *)

val dummy : t
(** Dummy output, drops everything written to it. *)

val of_unix_fd : ?close_noerr:bool -> ?buf:bytes -> Unix.file_descr -> t
(** Output stream writing into the given Unix file descriptor. *)

val of_buffer : Buffer.t -> t
(** [of_buffer buf] is an output channel that writes directly into [buf].
    [flush] and [close] have no effect. *)

val output_char : #t -> char -> unit
(** Output a single char *)

val output : #t -> bytes -> int -> int -> unit
(** Write the slice of bytes. *)

val close : #t -> unit
(** Close the stream. Idempotent. *)

val flush : #t -> unit
(** Ensure the bytes written so far are indeed written to the underlying
      storage/network socket/… and are not just sitting in a buffer. *)

val output_string : #t -> string -> unit
(** Output the whole string. *)

val output_line : #t -> string -> unit
(** Output the whole string followed by ['\n'].
    @since 0.2 *)

val output_lines : #t -> string Seq.t -> unit
(** Output a series of lines, each terminated by ['\n']. *)

val output_int : #t -> int -> unit
(** Output an integer in decimal notation. *)

val tee : t list -> t
(** [tee ocs] is an output that accepts bytes and writes them to every output
    in [ocs]. When closed, it closes all elements of [oc]. *)