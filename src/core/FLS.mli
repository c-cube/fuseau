(** Fiber-local storage.

    This storage is associated to the current fiber,
    just like thread-local storage is associated with
    the current thread.
*)

type 'a key
(** A key used to access a particular (typed) storage slot on
    every fiber. *)

val new_key : init:(unit -> 'a) -> unit -> 'a key
(** [new_key ~init ()] makes a new key. Keys are expensive and
    should never be allocated dynamically or in a loop.
    The correct pattern is, at toplevel:

    {[
      let k_foo : foo FLS.key = FLS.new_key ~init:(fun () -> make_foo ()) ()

    (* … *)

    (* use it: *)
    let _ = FLS.get k_foo
    ]}
*)

val get : 'a key -> 'a
(** Get the value for this key in the current fiber.
    Only call from inside a fiber *)

val set : 'a key -> 'a -> unit
(** Set the value for this key in the current fiber.
    Only call from inside a fiber *)

val with_value : 'a key -> 'a -> (unit -> 'b) -> 'b
