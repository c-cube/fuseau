# Fuseau

This is an experimental concurrency/IO library for OCaml 5. Currently the only backend is a naive one
using `Unix.select` but I plan to add more robust ones.

A basic example that spins 4 concurrent loops in direct style:

```ocaml
let spf = Printf.sprintf

let main () =
  print_endline "entering main";

  let computations =
    Array.init 4 (fun j ->
        Fuseau.spawn @@ fun () ->
        print_endline @@ spf "spawn fiber %d" j;
        for i = 1 to 20 do
          (* this suspends the fiber for 500ms *)
          Fuseau.sleep 0.5;
          print_endline @@ spf "iter %d (fiber %d)" i j
        done)
  in

  (* wait for the loops to end *)
  Array.iter Fuseau.await computations;
  print_endline "done"

let () = Fuseau.main main
```

## License

MIT licensed.

## Name

Fuseau ("fuh-zo") is french for a spindle. It's vaguely related to fibers :-)

