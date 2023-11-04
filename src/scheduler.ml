open Common_
module ED = Effect.Deep

type task = unit -> unit

type t = {
  active: bool Atomic.t;
  cur_q: task Queue.t;
  next_q: task Queue.t;
  outside_q: task Queue.t Lock.t;  (** Queue of tasks from the outside *)
  tid: int;  (** Current thread *)
  mutable n_tasks: int;  (** Total number of tasks processed since the start *)
}

let k_current_scheduler : t option ref TLS.key =
  TLS.new_key (fun () -> ref None)

let[@inline] schedule_for_this_round_ (self : t) (f : task) : unit =
  Queue.push f self.cur_q

let[@inline] schedule_for_next_round_ (self : t) (task : task) : unit =
  Queue.push task self.next_q

let[@inline] active self = Atomic.get self.active
let[@inline] n_tasks self = self.n_tasks

let has_pending_tasks self : bool =
  not
    (Queue.is_empty self.cur_q && Queue.is_empty self.next_q
    && Lock.map_no_exn Queue.is_empty self.outside_q)

module Private = struct
  let has_pending_tasks = has_pending_tasks
  let k_current_scheduler = k_current_scheduler
end

let create () : t =
  {
    tid = Thread.id @@ Thread.self ();
    active = Atomic.make true;
    cur_q = Queue.create ();
    next_q = Queue.create ();
    outside_q = Lock.create @@ Queue.create ();
    n_tasks = 0;
  }

let[@inline] dispose (self : t) : unit = Atomic.set self.active false

let[@inline] as_disposable self =
  { Disposable.dispose = (fun () -> dispose self) }

(** Run the next task, if any *)
let run_next (self : t) : unit =
  match Queue.pop self.cur_q with
  | work -> work ()
  | exception Queue.Empty -> ()

type 'a with_cur_fiber = {
  scheduler: t;
  fiber: 'a Fiber.t;
}

(** Schedule this fiber, after the trigger was signaled *)
let resume (self : _ with_cur_fiber) (trigger : _ Trigger.t) fiber k : unit =
  if not (Fiber.has_forbidden fiber) then Fiber.detach fiber trigger;
  let work () = Effect.Deep.continue k (Fiber.canceled fiber) in
  schedule_for_this_round_ self.scheduler work

let rec fork (self : _ with_cur_fiber) (main : task) : unit =
  self.scheduler.n_tasks <- 1 + self.scheduler.n_tasks;
  let current = Some (fun k -> Fiber.continue self.fiber k self.fiber)
  and yield =
    Some
      (fun k ->
        schedule_for_next_round_ self.scheduler (Fiber.continue self.fiber k);
        run_next self.scheduler)
  in

  let effc (type a) : a Effect.t -> ((a, _) ED.continuation -> _) option =
    function
    | Fiber.Current -> current
    | Fiber.Spawn { forbid; computation; mains } ->
      Some (handle_spawn self ~forbid computation mains)
    | Fiber.Yield -> yield
    | Trigger.Await trigger -> Some (handle_await self trigger)
    | _ -> None
  and retc () = run_next self.scheduler in
  Effect.Deep.match_with main () { retc; exnc = raise; effc }

(* spawn sub-fibers *)
and handle_spawn :
    type a ret.
    _ with_cur_fiber ->
    forbid:bool ->
    a Computation.as_cancelable ->
    task list ->
    (unit, ret) ED.continuation ->
    ret =
 fun self ~forbid computation mains k ->
  match Fiber.canceled self.fiber with
  | None ->
    (* parent fiber is not cancelled, we can proceed
       by scheduling each function in [mains] *)
    List.iter
      (fun main ->
        let fiber = Fiber.create ~forbid computation in
        schedule_for_this_round_ self.scheduler (fun () ->
            fork { self with fiber } main))
      mains;
    (* and resume parent fiber *)
    ED.continue k ()
  | Some exn_bt -> Exn_bt.discontinue k exn_bt

and handle_await :
    _ with_cur_fiber ->
    _ Trigger.t ->
    (Exn_bt.t option, unit) ED.continuation ->
    unit =
 fun self trigger k ->
  if Fiber.has_forbidden self.fiber then (
    (* Fiber has forbidden propagation of cancelation. *)
    let did_register_trigger =
      Trigger.on_signal trigger self.fiber k (resume self)
    in
    if did_register_trigger then
      (* Fiber is now suspended and can be resumed through the
         trigger. We just continue the next ready fiber. *)
      run_next self.scheduler
    else
      (* The trigger was already signaled, just continue. *)
      ED.continue k None
  ) else if
      (* Fiber permits propagation of cancelation. We support
         cancelation and so first try to attach the trigger to the
         computation of the fiber. *)
      Fiber.try_attach self.fiber trigger
    then
    (* The trigger was successfully attached, which means the
       computation has not been canceled. *)
    if Trigger.on_signal trigger self.fiber k (resume self) then
      (* Fiber is now suspended and can be resumed through the
         trigger.  That can now happen by signaling the trigger
         directly or by canceling the computation of the fiber,
         which will also signal the trigger.  We just continue the
         next ready fiber. *)
      run_next self.scheduler
    else (
      (* The trigger was already signaled.  We first need to
         ensure that the trigger is detached from the computation
         of the fiber. *)
      Fiber.detach self.fiber trigger;
      (* We could now freely decide which fiber to continue, but
         in this scheduler we choose to continue the current
         fiber. *)
      ED.continue k (Fiber.canceled self.fiber)
    )
  else (
    (* We could not attach the trigger to the computation of the
       fiber, which means that either the computation has been
       canceled or the trigger has been signaled.  We still need
       to ensure that the trigger really is signaled. *)
    Trigger.signal trigger;
    (* We could now freely decide which fiber to continue, but in
       this scheduler we choose to continue the current fiber. *)
    ED.continue k (Fiber.canceled self.fiber)
  )

let run_single_fun (self : t) ~forbid (f : unit -> 'a) :
    ('a, [ `Await | `Cancel ]) Computation.t =
  let computation = Computation.create () in
  let fiber = Fiber.create ~forbid computation in
  let work () =
    fork { scheduler = self; fiber } (Computation.capture computation f)
  in
  schedule_for_next_round_ self work;
  (computation :> (_, [ `Await | `Cancel ]) Computation.t)

let run_iteration (self : t) : unit =
  Queue.transfer self.next_q self.cur_q;
  Lock.with_ self.outside_q (fun q -> Queue.transfer q self.cur_q);
  run_next self

(** Scheduler for the current thread *)
let get_sched_for_cur_thread_ () : t =
  match !(TLS.get k_current_scheduler) with
  | None -> failwith "must be run from inside the event loop"
  | Some sch -> sch

let spawn f : (_, [ `Await | `Cancel ]) Computation.t =
  let self = get_sched_for_cur_thread_ () in
  run_single_fun self ~forbid:false f

let spawn_from_anywhere (self : t) f : _ Computation.t =
  if Thread.id (Thread.self ()) = self.tid then
    run_single_fun self ~forbid:false f
  else (
    let computation = Computation.create () in
    let fiber = Fiber.create ~forbid:false computation in
    let work () =
      fork { scheduler = self; fiber } (Computation.capture computation f)
    in
    Lock.with_ self.outside_q (fun q -> Queue.push work q);
    (computation :> (_, [ `Await | `Cancel ]) Computation.t)
  )

let schedule_micro_task (f : unit -> unit) : unit =
  let self = get_sched_for_cur_thread_ () in
  schedule_for_this_round_ self f
