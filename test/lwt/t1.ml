let pf = Printf.printf
let ( let@ ) = ( @@ )

let run () =
  Fuseau_lwt.main @@ fun () ->
  pf "start\n%!";
  Fuseau.sleep_s 0.5;
  pf "done\n%!";
  42

let () =
  let@ () = Trace_tef.with_setup () in
  let n = run () in
  pf "res: %d\n" n
