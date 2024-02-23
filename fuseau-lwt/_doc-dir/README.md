# Fuseau

This is an experimental concurrency/IO library for OCaml 5. Currently the only backend is a naive one
using `Unix.select` but I plan to add more robust ones.

A basic example that spins 4 concurrent loops in direct style:

```ocaml
# let spf = Printf.sprintf
val spf : ('a, unit, string) format -> 'a = <fun>

# let main () =
  print_endline "entering main";

  let computations =
    Array.init 4 (fun j ->
        Fuseau.spawn @@ fun () ->
        print_endline @@ spf "spawn fiber %d" j;
        for i = 1 to 3 do
          (* this suspends the fiber for 500ms *)
          Fuseau.sleep_s 0.5;
          print_endline @@ spf "iter %d (fiber %d)" i j
        done)
  in

  (* wait for the loops to end *)
  Array.iter Fuseau.await computations;
  print_endline "done"
val main : unit -> unit = <fun>

# let () = Fuseau_unix.main main
entering main
spawn fiber 0
spawn fiber 1
spawn fiber 2
spawn fiber 3
iter 1 (fiber 0)
iter 1 (fiber 1)
iter 1 (fiber 2)
iter 1 (fiber 3)
iter 2 (fiber 0)
iter 2 (fiber 1)
iter 2 (fiber 2)
iter 2 (fiber 3)
iter 3 (fiber 0)
iter 3 (fiber 1)
iter 3 (fiber 2)
iter 3 (fiber 3)
done
```

## License

MIT licensed.

## Name

Fuseau ("fuh-zo") is french for a spindle. It's vaguely related to fibers :-)

