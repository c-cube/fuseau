(** Scheduler for our fibers. *)

type t

val create : ?max_tick_duration_us:int -> ev_loop:Event_loop.t -> unit -> t
(** New scheduler.
   @param max_tick_duration_us maximum duration, in microseconds, of the
   computational part of a tick (ie. the part where we run fibers
   without polling IOs) *)

val active : t -> bool

exception Inactive
(** Exception raised when trying to perform operations
    on the scheduler after it's been disposed of *)

val dispose : t -> unit
(** Delete the scheduler. Idempotent and thread-safe. *)

val spawn : ?propagate_cancel_to_parent:bool -> (unit -> 'a) -> 'a Fiber.t
(** Must be run from inside the scheduler's thread. Spawn a new computation.
    @raise Inactive if the scheduler is inactive. *)

val schedule_micro_task : (unit -> unit) -> unit
(** Must be run from inside the scheduler's thread. Schedules a microtask
    that will run in this tick. Be careful not to create infinite sequences
    of micro tasks that starve the IO loop!

    These microtasks do not handle effects and should try their best to
    not raise exceptions. Only use them for very short amount of work.

    Not thread-safe.
    @raise Inactive if the scheduler is inactive. *)

val spawn_from_anywhere : t -> (unit -> 'a) -> 'a Fiber.t
(** Spawn a task from anywhere, possibly from another thread. The task will
    run in a subsequent call to {!run_iteration} in the scheduler's thread.
    Thread-safe, more costly than {!spawn}. Runs under the root switch.
    @raise Inactive if the scheduler is inactive. *)

val n_tasks_since_beginning : t -> int
(** Number of tasks run so far. *)

val run_iteration : t -> unit
(** Run one iteration of the scheduler, until the quota of tasks has
    been met or no fiber is runnable.
    @raise Inactive if the scheduler is inactive. *)

(**/**)

module Internal_ : sig
  open Common_

  val has_pending_tasks : t -> bool
  (** Are there currently tasks that are ready to run? *)

  val ev_loop : t -> Event_loop.t
  val k_current_scheduler : t option ref TLS.key
end

(**/**)
