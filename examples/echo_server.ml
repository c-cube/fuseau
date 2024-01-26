let ( let@ ) = ( @@ )
let pf = Printf.printf

let main ~port () =
  pf "serve on localhost:%d\n%!" port;

  let addr = Fuseau.Net.Sockaddr.inet_local port in
  let@ server =
    Fuseau.Net.TCP_server.with_serve addr (fun client_addr ic oc ->
        pf "handle client on %s\n%!" (Fuseau.Net.Sockaddr.show client_addr);

        let buf = Bytes.create 256 in
        let continue = ref true in
        while !continue do
          let n = Fuseau.IO_in.input ic buf 0 (Bytes.length buf) in
          if n = 0 then
            continue := false
          else (
            Fuseau.IO_out.output oc buf 0 n;
            Fuseau.IO_out.flush oc
          )
        done;
        pf "done with client on %s\n%!" (Fuseau.Net.Sockaddr.show client_addr))
  in
  Fuseau.Net.TCP_server.join server;
  print_endline "exit"

let () =
  let@ () = Trace_tef.with_setup () in
  let port = ref 5656 in
  let opts = [ "-p", Arg.Set_int port, " port" ] |> Arg.align in
  Arg.parse opts ignore "echo_server";

  let loop = new Fuseau_unix.ev_loop in
  Fuseau.main ~loop (main ~port:!port)
