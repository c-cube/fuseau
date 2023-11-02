module L2L = Luv2loop

let spf = Printf.sprintf

let main () =
  print_endline "hello";

  let fibers =
    Array.init 4 (fun j ->
        L2L.Fiber.spawn @@ fun () ->
        for i = 1 to 100 do
          L2L.Timer.sleep_ms 100;
          print_endline @@ spf "iter %d (fiber %d)" i j
        done)
  in

  Array.iter L2L.Fiber.await fibers;
  print_endline "done"

let () = L2L.main main
