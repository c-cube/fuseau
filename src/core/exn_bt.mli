(** Exception with backtrace *)

type t = {
  exn: exn;
  bt: Printexc.raw_backtrace;
}

val make : exn -> Printexc.raw_backtrace -> t
val get : exn -> t
val get_callstack : int -> exn -> t
val raise : t -> 'a
val discontinue : ('a, 'b) Effect.Deep.continuation -> t -> 'b

val discontinue_with :
  ('a, 'b) Effect.Shallow.continuation ->
  t ->
  ('b, 'c) Effect.Shallow.handler ->
  'c

type nonrec 'a result = ('a, t) result
