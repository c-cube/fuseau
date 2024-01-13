open Common_
open Fuseau_core

module Sockaddr = struct
  type t = Unix.sockaddr

  let show = function
    | Unix.ADDR_UNIX s -> s
    | Unix.ADDR_INET (addr, port) ->
      spf "%s:%d" (Unix.string_of_inet_addr addr) port

  let unix s : t = Unix.ADDR_UNIX s
  let inet addr port : t = Unix.ADDR_INET (addr, port)
  let inet_parse addr port = inet (Unix.inet_addr_of_string addr) port
  let inet_local port = inet Unix.inet_addr_loopback port
  let inet_any port = inet Unix.inet_addr_any port
end

module TCP_server = struct
  type t = { fiber: unit Fiber.t } [@@unboxed]

  exception Stop

  let stop_ fiber =
    let ebt = Exn_bt.get Stop in
    Fiber.Internal_.cancel fiber ebt

  let stop self = stop_ self.fiber
  let join self = Fiber.await self.fiber

  let with_serve (addr : Sockaddr.t) handle_client (f : t -> 'a) : 'a =
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

    Unix.bind sock addr;
    Unix.set_nonblock sock;
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    Unix.listen sock 32;

    let fiber = Fiber.Internal_.create () in
    let self = { fiber } in

    let sched = get_sched "tcp_server.with_serve" () in

    let loop_client client_sock client_addr : unit =
      Unix.set_nonblock client_sock;
      Unix.setsockopt client_sock Unix.TCP_NODELAY true;

      let ic = IO_in.of_unix_fd client_sock in
      let oc = IO_out.of_unix_fd client_sock in
      let@ () =
        Fun.protect ~finally:(fun () ->
            IO_in.close ic;
            IO_out.close oc)
      in
      handle_client client_addr ic oc
    in

    let loop () =
      while not (Fiber.is_done fiber) do
        match Unix.accept sock with
        | client_sock, client_addr ->
          ignore
            (spawn ~propagate_cancel_to_parent:false (fun () ->
                 loop_client client_sock client_addr)
              : _ Fiber.t)
        | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
          (* suspend *)
          Fiber.Internal_.suspend ~before_suspend:(fun ~wakeup ->
              (* FIXME: possible race condition: the socket became readable
                  in the mid-time and we won't get notified. We need to call
                  [accept] after subscribing to [on_readable]. *)
              let loop = Scheduler.Internal_.ev_loop sched in
              ignore
                (loop#on_readable sock (fun _ev -> wakeup ()) : event_handle))
      done
    in

    let loop_fiber =
      spawn_as_child_of ~propagate_cancel_to_parent:true sched fiber loop
    in
    let finally () =
      stop_ loop_fiber;
      Unix.close sock
    in
    let@ () = Fun.protect ~finally in
    f self
end
