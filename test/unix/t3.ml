let pf = Printf.printf
let ( let@ ) = ( @@ )

let rec fib n =
  if n <= 2 then
    1
  else (
    let f1 = Fuseau.spawn (fun () -> fib (n - 1)) in
    let f2 = Fuseau.spawn (fun () -> fib (n - 2)) in

    if n = 9 then Fuseau.sleep_s 0.000_010;
    Fuseau.await f1 + Fuseau.await f2
  )

let run () =
  Fuseau_unix.main @@ fun () ->
  pf "start\n%!";

  let fib10 = Fuseau.spawn ~name:"fib10" (fun () -> fib 10) in

  let fib14 = Fuseau.spawn ~name:"fib14" (fun () -> fib 14) in
  pf "fib10=%d\n" (Fuseau.await fib10);

  pf "fib14=%d\n" (Fuseau.await fib14);

  let fib30 =
    Fuseau.spawn ~name:"fib30" ~propagate_cancel_to_parent:false (fun () ->
        let@ () =
          Fuseau.with_cancel_callback (fun ebt ->
              Trace.message "fib30 cancelled";
              pf "fib30 cancelled with %s\n%!" (Fuseau.Exn_bt.show ebt))
        in

        Fuseau.cancel_after_s 0.002;
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
