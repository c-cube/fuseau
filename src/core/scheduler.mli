(** Scheduler for our fibers. *)

type t

val create : ev_loop:Event_loop.t -> unit -> t
(** New scheduler. *)

val active : t -> bool
(** Is the scheduler not finished yet? *)

exception Inactive
(** Exception raised when trying to perform operations
    on the scheduler after it's been disposed of *)

val dispose : t -> unit
(** Delete the scheduler. Idempotent and thread-safe.
    This cancels the remaining fibers. The scheduler will
    stop running when they all terminate. *)

val schedule_micro_task : (unit -> unit) -> unit
(** Must be run from inside the scheduler's thread. Schedules a microtask
    that will run in this tick. Be careful not to create infinite sequences
    of micro tasks that starve the IO loop!

    These microtasks do not handle effects and should try their best to
    not raise exceptions. Only use them for very short amount of work.

    Not thread-safe.
    @raise Inactive if the scheduler is inactive. *)

val spawn : ?propagate_cancel_to_parent:bool -> (unit -> 'a) -> 'a Fiber.t
(** Must be run from inside the scheduler's thread. Spawn a new computation.
    This fiber has an implicit parent, which is normally the currently running
    fiber (the one calling {!spawn}). If the parent fails or is cancelled, the
    resulting fiber will also be cancelled (parent to child).
    @param propagate_cancel_to_parent if true (the default), if this fiber fails
      then the parent fiber will also fail (child to parent).
    @raise Inactive if the scheduler is inactive. *)

val spawn_from_anywhere : t -> (unit -> 'a) -> 'a Fiber.t
(** Spawn a task from anywhere, possibly from another thread. The task will
    run in a subsequent call to {!run_iteration} in the scheduler's thread.
    Thread-safe, more costly than {!spawn}. Runs under the root switch.
    @raise Inactive if the scheduler is inactive. *)

val spawn_as_child_of :
  ?propagate_cancel_to_parent:bool ->
  t ->
  _ Fiber.t ->
  (unit -> 'a) ->
  'a Fiber.t
(** Spawn a fiber in the given parent fiber's scope.
    See {!spawn} for more details on the arguments *)

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

  val check_active : t -> unit
  val ev_loop : t -> Event_loop.t
  val k_current_scheduler : t option ref TLS.key
end

(**/**)
