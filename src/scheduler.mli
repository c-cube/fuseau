(** Scheduler for our fibers. *)

type t

val create : loop:Luv.Loop.t -> unit -> t
(** New scheduler. *)

val active : t -> bool

val dispose : t -> unit
(** Delete the scheduler. Idempotent *)

val as_disposable : t -> Disposable.t

val spawn : (unit -> 'a) -> ('a, [ `Await | `Cancel ]) Computation.t
(** Must be run from inside the scheduler's thread. Spawn a new computation. *)

val schedule_micro_task : (unit -> unit) -> unit
(** Must be run from inside the scheduler's thread. Schedules a microtask
    that will run in this tick. Be careful not to create infinite sequences
    of micro tasks that starve the IO loop! *)

val spawn_from_anywhere :
  t -> (unit -> 'a) -> ('a, [ `Await | `Cancel ]) Computation.t
(** Spawn a task from anywhere, possibly from another thread.
    Thread-safe. *)

val n_tasks : t -> int
(** Number of tasks run so far *)

val run_iteration : t -> unit
(** Run one iteration of the scheduler, until no immediately
    runnable task remains. *)

module Private : sig
  open Common_

  val has_pending_tasks : t -> bool
  (** Are there currently tasks that are ready to run? *)

  val k_current_scheduler : t option ref TLS.key
end
