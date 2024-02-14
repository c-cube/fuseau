module F = Fuseau_unix

let ( let@ ) = ( @@ )
let pf = Printf.printf
let verbose = ref false

let main ~port () =
  pf "serve on localhost:%d\n%!" port;

  let addr = F.Net.Sockaddr.inet_local port in
  let@ server =
    F.Net.TCP_server.with_serve addr (fun client_addr ic oc ->
        if !verbose then
          pf "handle client on %s\n%!" (F.Net.Sockaddr.show client_addr);

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
        if !verbose then
          pf "done with client on %s\n%!" (F.Net.Sockaddr.show client_addr))
  in
  F.Net.TCP_server.join server;
  print_endline "exit"

let () =
  let@ () = Trace_tef.with_setup () in
  let port = ref 1234 in
  let opts =
    [ "-p", Arg.Set_int port, " port"; "-v", Arg.Set verbose, " verbose" ]
    |> Arg.align
  in
  Arg.parse opts ignore "echo_server";

  F.main (main ~port:!port)
