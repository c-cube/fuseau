module F = Fuseau_lwt
module Trace = Trace_core

let ( let@ ) = ( @@ )
let spf = Printf.sprintf

let str_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (addr, port) ->
    spf "%s:%d" (Unix.string_of_inet_addr addr) port

let main ~port () : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "main" in

  let lwt_fut, _lwt_prom = Lwt.wait () in

  (* TODO: handle exit?? *)
  Printf.printf "listening on port %d\n%!" port;

  let handle_client client_addr ic oc =
    let _client_sp =
      Trace.enter_manual_toplevel_span ~__FILE__ ~__LINE__ "handle.client"
        ~data:(fun () -> [ "addr", `String (str_of_sockaddr client_addr) ])
    in

    let buf = Bytes.create 32 in
    let continue = ref true in
    while !continue do
      let _sp =
        Trace.enter_manual_sub_span ~parent:_client_sp ~__FILE__ ~__LINE__
          "read.loop"
      in
      Trace.message "read";
      let n = F.Iostream.In.input ic buf 0 (Bytes.length buf) in
      if n = 0 then
        continue := false
      else (
        Trace.messagef (fun k -> k "got %dB" n);
        F.Iostream.Out.output oc buf 0 n;
        F.Iostream.Out.flush oc;
        Trace.message "write" (* MU.sleep_s 0.02 *)
      );
      Trace.exit_manual_span _sp
    done;
    Trace.exit_manual_span _client_sp
  in

  (* TODO: catch errors?
     let handle_client client_addr ic oc =
       try handle_client client_addr ic oc
       with e -> Printf.eprintf "uncaught exn: %s\n%!" (Printexc.to_string e)
     in
  *)
  let addr = Unix.ADDR_INET (Unix.inet_addr_any, port) in
  let _server = F.Net.TCP_server.establish addr handle_client in

  F.await_lwt lwt_fut

let () =
  let@ () = Trace_tef.with_setup () in
  Trace.set_thread_name "main";
  let port = ref 1234 in

  let opts = [ "-p", Arg.Set_int port, " port" ] |> Arg.align in
  Arg.parse opts ignore "echo server";

  Lwt_engine.set @@ new Lwt_engine.libev ();
  F.main @@ main ~port:!port
