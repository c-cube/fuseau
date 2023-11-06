open Utils_

type t = [ `TCP ] Luv_stream.t

module Address_family = Sockaddr.Address_family

let init ?(domain : Address_family.t option) () : t =
  let loop = get_cur_luv_loop () in
  Luv.TCP.init ~loop ?domain () |> unwrap_luv_res

let open_ (self : t) (sock : Os_fd.Socket.t) : unit =
  Luv.TCP.open_ self sock |> unwrap_luv_res

module Flag = Luv.TCP.Flag

let socketpair ?fst_flags ?snd_flags (st : Sockaddr.Socket_type.t) kind :
    Os_fd.Socket.t * Os_fd.Socket.t =
  Luv.TCP.socketpair ?fst_flags ?snd_flags st kind |> unwrap_luv_res

let nodelay self b : unit = Luv.TCP.nodelay self b |> unwrap_luv_res
let keepalive self b : unit = Luv.TCP.keepalive self b |> unwrap_luv_res

let simultaneous_accepts self b : unit =
  Luv.TCP.simultaneous_accepts self b |> unwrap_luv_res

let bind ?ipv6only (self : t) (addr : Sockaddr.t) : unit =
  Luv.TCP.bind ?ipv6only self addr |> unwrap_luv_res

let getsockname (self : t) : Sockaddr.t =
  Luv.TCP.getsockname self |> unwrap_luv_res

let getpeername (self : t) : Sockaddr.t =
  Luv.TCP.getpeername self |> unwrap_luv_res

let connect self (addr : Sockaddr.t) : unit =
  await_cb_ () @@ fun k -> Luv.TCP.connect self addr k

let close_reset self : unit =
  await_cb_ () @@ fun k -> Luv.TCP.close_reset self k
