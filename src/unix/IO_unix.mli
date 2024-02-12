(** Low level Unix IOs *)

type file_descr = Unix.file_descr

val read : file_descr -> bytes -> int -> int -> int
val write_once : file_descr -> bytes -> int -> int -> int
val write : file_descr -> bytes -> int -> int -> unit

module IO_in : sig
  include module type of IO_in

  val of_unix_fd : ?close_noerr:bool -> ?buf:bytes -> file_descr -> t
end

module IO_out : sig
  include module type of IO_out

  val of_unix_fd : ?close_noerr:bool -> ?buf:bytes -> file_descr -> t
end
