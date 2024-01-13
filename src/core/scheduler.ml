open Common_

type microtask = unit -> unit

type task =
  | T_start : 'a Fiber.t * (unit -> 'a) -> task
  | T_cont : Fiber.any * ('a, unit) ED.continuation * 'a -> task

type t = {
  root_fiber: Fiber.any;  (** Fiber encompassing all other *)
  ev_loop: Event_loop.t;
  mutable cur_fiber: Fiber.any option;  (** Currently running fiber *)
  task_q: task Queue.t;  (** Queue of regular tasks *)
  next_tick_tasks: task Queue.t;  (** Tasks for the next tick *)
  micro_tasks_q: microtask Queue.t;  (** Microtasks, run almost immediately *)
  outside_q: task Queue.t Lock.t;
      (** Queue of tasks from the outside (from other threads possibly) *)
  mutable n_tasks: int;  (** Total number of tasks processed since the start *)
}

let k_current_scheduler : t option ref TLS.key =
  TLS.new_key (fun () -> ref None)

let () =
  (* to get current fiber, just get current scheduler *)
  Fiber.Internal_.get_current :=
    fun () ->
      let cur_sched = TLS.get k_current_scheduler in
      match !cur_sched with
      | Some s -> s.cur_fiber
      | None -> None

exception Inactive

let[@inline] check_active_ (self : t) =
  let (Any_fiber f) = self.root_fiber in
  if Fiber.is_done f then raise Inactive

let[@inline] schedule_ (self : t) (task : task) : unit =
  Trace.message "schedule";
  check_active_ self;
  Queue.push task self.next_tick_tasks

let[@inline] schedule_micro_task_ (self : t) f : unit =
  check_active_ self;
  Queue.push f self.micro_tasks_q

let[@inline] active self =
  let (Any_fiber f) = self.root_fiber in
  not (Fiber.is_done f)

let[@inline] n_tasks_since_beginning self = self.n_tasks

let[@inline] has_pending_tasks self : bool =
  not
    (Queue.is_empty self.task_q
    && Queue.is_empty self.micro_tasks_q
    && Queue.is_empty self.next_tick_tasks
    && Lock.map_no_exn Queue.is_empty self.outside_q)

module Internal_ = struct
  let has_pending_tasks = has_pending_tasks
  let k_current_scheduler = k_current_scheduler
  let check_active = check_active_
  let[@inline] ev_loop self = self.ev_loop
end

let create ~ev_loop () : t =
  {
    ev_loop;
    cur_fiber = None;
    task_q = Queue.create ();
    next_tick_tasks = Queue.create ();
    root_fiber = Any_fiber (Fiber.Internal_.create ());
    micro_tasks_q = Queue.create ();
    outside_q = Lock.create @@ Queue.create ();
    n_tasks = 0;
  }

let dispose (self : t) : unit =
  (* cancel the main task *)
  let ebt = Exn_bt.get_callstack 15 Exit in
  Fiber.Internal_.cancel_any self.root_fiber ebt

(** Scheduler for the current thread *)
let[@inline] get_sched_for_cur_thread_ () : t =
  match !(TLS.get k_current_scheduler) with
  | None -> failwith "must be run from inside the event loop"
  | Some sch -> sch

let[@inline] schedule_micro_task (f : unit -> unit) : unit =
  let self = get_sched_for_cur_thread_ () in
  schedule_micro_task_ self f

let spawn ?(propagate_cancel_to_parent = true) (f : unit -> 'a) : 'a Fiber.t =
  let self : t = get_sched_for_cur_thread_ () in
  check_active_ self;

  (* build a switch for the fiber *)
  let (Any_fiber parent) =
    match self.cur_fiber with
    | None -> self.root_fiber
    | Some s -> s
  in

  let fiber = Fiber.Internal_.create () in
  Fiber.Internal_.add_child
    ~protected:(not propagate_cancel_to_parent)
    parent fiber;
  schedule_ self (T_start (fiber, f));
  fiber

let spawn_as_child_of ?(propagate_cancel_to_parent = true) (self : t)
    (parent : _ Fiber.t) f : _ Fiber.t =
  check_active_ self;
  let fiber = Fiber.Internal_.create () in
  Fiber.Internal_.add_child
    ~protected:(not propagate_cancel_to_parent)
    parent fiber;
  schedule_ self (T_start (fiber, f));
  fiber

let spawn_from_anywhere (self : t) f : _ Fiber.t =
  check_active_ self;
  let (Any_fiber parent) = self.root_fiber in
  let fiber = Fiber.Internal_.create () in
  Fiber.Internal_.add_child ~protected:true parent fiber;
  Lock.with_ self.outside_q (fun q -> Queue.push (T_start (fiber, f)) q);
  fiber

(** call [f()] and resolve the fiber once [f()] is done *)
let run_task_and_resolve_fiber fiber f =
  try
    let r = f () in
    Fiber.Internal_.resolve fiber r
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    Trace.messagef (fun k -> k "fiber raised %s" (Printexc.to_string exn));
    Fiber.Internal_.cancel fiber (Exn_bt.make exn bt)

let run_task (self : t) (task : task) : unit =
  Trace.message "sched.run-task";
  match task with
  | T_start (fiber, f) ->
    (* the main effect handler *)
    let effc : type b. b Effect.t -> ((b, unit) ED.continuation -> 'a) option =
      function
      | Effects.Suspend { before_suspend } ->
        Some
          (fun k ->
            let wakeup () =
              Trace.message "wakeup suspended fiber";
              schedule_ self (T_cont (Any_fiber fiber, k, ()))
            in
            before_suspend ~wakeup)
      | Effects.Yield ->
        Some (fun k -> schedule_ self (T_cont (Any_fiber fiber, k, ())))
      | _ -> None
    in

    (* whole fiber runs under the effect handler *)
    self.cur_fiber <- Some (Any_fiber fiber);
    (try ED.try_with (run_task_and_resolve_fiber fiber) f { ED.effc }
     with exn -> Printf.eprintf "fiber raised %s\n%!" (Printexc.to_string exn))
  | T_cont ((Any_fiber fib as any_fib), k, x) ->
    self.cur_fiber <- Some any_fib;
    (match Fiber.peek fib with
    | Fail ebt ->
      (* cleanup *)
      Exn_bt.discontinue k ebt
    | Done _ -> assert false
    | Wait _ ->
      (* continue running the fiber *)
      self.cur_fiber <- Some any_fib;
      ED.continue k x)

let run_iteration (self : t) : unit =
  let@ _sp =
    Trace.with_span ~__FILE__ ~__LINE__ "sched.run_iteration" ~data:(fun () ->
        [ "n-tasks", `Int (Queue.length self.task_q) ])
  in
  check_active_ self;

  (* move all pending tasks to [task_q] *)
  Queue.transfer self.next_tick_tasks self.task_q;
  Lock.with_ self.outside_q (fun q -> Queue.transfer q self.task_q);

  while not (Queue.is_empty self.task_q) do
    let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "sched.iter" in

    (* run microtasks *)
    while not (Queue.is_empty self.micro_tasks_q) do
      let f = Queue.pop self.micro_tasks_q in
      try f ()
      with e ->
        Printf.eprintf "warning: microtask raised %s\n%!" (Printexc.to_string e)
    done;

    let task = Queue.pop self.task_q in
    self.n_tasks <- 1 + self.n_tasks;

    run_task self task;
    (* cleanup *)
    self.cur_fiber <- None
  done
