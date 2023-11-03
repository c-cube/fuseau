(** Scheduler for our fibers. *)

type t

val create : unit -> t
(** New scheduler. *)

val active : t -> bool

val dispose : t -> unit
(** Delete the scheduler. Idempotent *)

val has_tasks : t -> bool
(** Are there currently waiting tasks? *)

val spawn : t -> (unit -> 'a) -> 'a Fiber.t
val schedule_micro_task : t -> (unit -> unit) -> unit

val n_tasks : t -> int
(** Number of tasks run so far *)

val run_iteration : t -> unit
(** Run one iteration of the scheduler, until no immediately
    runnable task remains. *)

val as_disposable : t -> Disposable.t
