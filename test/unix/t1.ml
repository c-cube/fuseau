let pf = Printf.printf
let ( let@ ) = ( @@ )

let run () =
  let loop = new Fuseau_unix.ev_loop in
  Fuseau.main ~loop @@ fun () ->
  Fuseau.sleep 0.5;
  42

let () =
  let@ () = Trace_tef.with_setup () in
  let n = run () in
  pf "res: %d\n" n
