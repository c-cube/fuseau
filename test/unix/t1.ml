let pf = Printf.printf
let ( let@ ) = ( @@ )

let run () =
  Fuseau_unix.main @@ fun () ->
  Fuseau.sleep_s 0.5;
  42

let () =
  let@ () = Trace_tef.with_setup () in
  let n = run () in
  pf "res: %d\n" n
