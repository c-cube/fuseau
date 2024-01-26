(** Networking *)

module Sockaddr : sig
  type t = Unix.sockaddr

  val show : t -> string
  val unix : string -> t
  val inet : Unix.inet_addr -> int -> t
  val inet_parse : string -> int -> t
  val inet_local : int -> t
  val inet_any : int -> t
end

module TCP_server : sig
  type t

  val stop : t -> unit
  val join : t -> unit

  val with_serve :
    Sockaddr.t -> (Sockaddr.t -> IO_in.t -> IO_out.t -> unit) -> (t -> 'a) -> 'a
end
