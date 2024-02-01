module F = Fuseau_lwt

let pf = Printf.printf
let ( let@ ) = ( @@ )

let main () =
  F.main @@ fun () ->
  pf "start\n%!";

  let fut1 =
    let open Lwt.Syntax in
    let* () = Lwt_unix.sleep 0.3 in
    pf "lwt sleeper1 is done!\n%!";
    Lwt.return 1
  in

  let fut2 =
    let open Lwt.Syntax in
    let* () = Lwt_unix.sleep 0.2 in
    pf "lwt sleeper2 is done!\n%!";
    Lwt.return 41
  in

  let simple_wait =
    F.spawn ~name:"fib_wait_2" @@ fun () ->
    pf "[simple] wait for fut2\n%!";
    let _y = F.await_lwt fut2 in
    pf "[simple] got %d from fut2\n%!" _y;
    ()
  in

  let fib_wait_both =
    F.spawn ~name:"fib_wait_both" @@ fun () ->
    pf "[both] wait for fut1\n%!";
    let x = F.await_lwt fut1 in
    pf "[both] wait for fut2\n%!";
    let y = F.await_lwt fut2 in
    pf "[both] fiber done\n%!";
    x + y
  in

  let res = F.await fib_wait_both in
  F.await simple_wait;

  Trace.message "done";
  pf "done\n%!";
  res

let () =
  let@ () = Trace_tef.with_setup () in
  let x = main () in
  pf "result: %d\n" x;
  pf "exit\n"
