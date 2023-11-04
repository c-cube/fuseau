# Fuseau

This is an experimental concurrency/IO library for OCaml 5.

It aims at being "boring" by reusing existing components that do the hard work:
- [Picos](https://github.com/ocaml-multicore/picos/) provides modern concurrency
    primitives (fibers, computations, triggers). These primitives are
    intended to be interoperable, fast, and correct. I also borrowed some
    code from @polytypic to implement the fiber scheduler.
- [Libuv](http://libuv.org) by way of the [Luv](https://github.com/aantron/luv) bindings,
    to handle IO primitives, timers, and general asynchronous interactions with the system.

Given these building blocks, Fuseau provides a direct-style API for Libuv's primitives
and concurrency (relying on Picos for these, e.g. `Computation.await : ('a, _) Computation.t -> 'a`).

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
          Fuseau.Timer.sleep_ms 500;
          print_endline @@ spf "iter %d (fiber %d)" i j
        done)
  in

  (* wait for the loops to end *)
  Array.iter Fuseau.Computation.await computations;
  print_endline "done"

let () = Fuseau.main main
```

## License

MIT licensed.

## Name

Fuseau ("fu-zo") is french for a spindle. It's vaguely related to fibers :-)

