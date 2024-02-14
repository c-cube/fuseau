let spf = Printf.sprintf

let main () =
  print_endline "hello";

  let comps =
    Array.init 4 (fun j ->
        Fuseau.spawn @@ fun () ->
        print_endline @@ spf "spawn fiber %d" j;
        for i = 1 to 10 do
          Fuseau.sleep_s 0.1;
          print_endline @@ spf "iter %d (fiber %d)" i j
        done)
  in

  Array.iter Fuseau.await comps;
  print_endline "done"

let () = Fuseau_unix.main main
