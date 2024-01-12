let pf = Printf.printf
let ( let@ ) = ( @@ )

let run () =
  let loop = new Fuseau_unix.ev_loop in
  Fuseau.main ~loop @@ fun () ->
  pf "start\n%!";
  let f1 =
    Fuseau.spawn (fun () ->
        Fuseau.sleep 0.5;
        pf "f1 returns\n%!";
        41)
  in
  let f2 =
    Fuseau.spawn (fun () ->
        Fuseau.sleep 0.3;
        pf "f2 returns\n%!";
        100)
  in

  Trace.message "await both";
  pf "await both\n%!";
  Fuseau.Fiber.await f1 + Fuseau.Fiber.await f2

let () =
  let@ () = Trace_tef.with_setup () in
  let n = run () in
  pf "res: %d\n" n
