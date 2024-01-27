let pf = Printf.printf
let ( let@ ) = ( @@ )

let run () =
  Fuseau_lwt.main @@ fun () ->
  pf "start\n%!";
  let f1 =
    Fuseau.spawn ~name:"f1" (fun () ->
        Fuseau.sleep_s 0.5;
        pf "f1 returns\n%!";
        41)
  in
  let f2 =
    Fuseau.spawn ~name:"f2" (fun () ->
        Fuseau.sleep_s 0.3;
        pf "f2 returns\n%!";
        100)
  in

  Trace.message "await both";
  pf "await both\n%!";
  Fuseau.await f1 + Fuseau.await f2

let () =
  let@ () = Trace_tef.with_setup () in
  let n = run () in
  pf "f2 returns\n%!";
  pf "res: %d\n" n
