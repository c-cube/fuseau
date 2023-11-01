(** Main loop *)

type t

val loop : t -> Luv.Loop.t
(** Luv loop *)

val spawn : t -> (unit -> 'a) -> 'a Fiber.t
(** Spawn a fiber *)

module Current : sig
  val get_exn : unit -> t
  (** Get local loop *)
end

val main : ?loop:Luv.Loop.t -> (unit -> 'a) -> 'a
