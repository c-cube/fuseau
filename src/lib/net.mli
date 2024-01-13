(** Networking *)

module Sockaddr : sig
  type t = Unix.sockaddr

  val show : t -> string
end

module TCP_server : sig
  type t

  val stop : t -> unit

  val with_serve :
    Sockaddr.t -> (Sockaddr.t -> IO_in.t -> IO_out.t -> unit) -> (t -> 'a) -> 'a
end
