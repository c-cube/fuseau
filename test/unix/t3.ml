let pf = Printf.printf
let ( let@ ) = ( @@ )

let rec fib n =
  if n <= 2 then
    1
  else (
    let f1 = Fuseau.spawn (fun () -> fib (n - 1)) in
    let f2 = Fuseau.spawn (fun () -> fib (n - 2)) in

    if n = 9 then Fuseau.sleep 0.000_010;
    Fuseau.await f1 + Fuseau.await f2
  )

let run () =
  let loop = new Fuseau_unix.ev_loop in
  Fuseau.main ~loop @@ fun () ->
  pf "start\n%!";

  let fib10 = Fuseau.spawn (fun () -> fib 10) in

  let fib14 = Fuseau.spawn (fun () -> fib 14) in
  pf "fib10=%d\n" (Fuseau.await fib10);

  pf "fib14=%d\n" (Fuseau.await fib14);

  let fib30 =
    Fuseau.spawn ~propagate_cancel_to_parent:false (fun () ->
        Fuseau.cancel_after 0.000_000_010;
        fib 30)
  in

  (match Fuseau.try_await fib30 with
  | Ok r -> pf "fib30 = %d\n" r
  | Error ebt -> pf "fib30 failed with %s\n" (Printexc.to_string ebt.exn));

  Trace.message "done";
  pf "done\n%!"

let () =
  let@ () = Trace_tef.with_setup () in
  run ();
  pf "exit\n"
