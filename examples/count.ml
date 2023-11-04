module L2L = Luv2loop

let spf = Printf.sprintf

let main () =
  print_endline "hello";

  let comps =
    Array.init 4 (fun j ->
        L2L.spawn @@ fun () ->
        print_endline @@ spf "spawn fiber %d" j;
        for i = 1 to 10 do
          L2L.Timer.sleep_ms 100;
          print_endline @@ spf "iter %d (fiber %d)" i j
        done)
  in

  Array.iter L2L.Computation.await comps;
  print_endline "done"

let () = L2L.main main
