open Common_

type task =
  | T : {
      k: ('a, unit) ES.continuation;
      arg: 'a;
      comp: _ Picos.Computation.packed;
    }
      -> task  (** A task, ie a continuation to run *)

type t = {
  active: bool Atomic.t;
  cur_q: task Queue.t;
  next_q: task Queue.t;
  mutable n_tasks: int;  (** Total number of tasks processed since the start *)
}

let basic_comp_ : [ `Await | `Cancel | `Return ] Picos.Computation.packed =
  Picos.Computation.Packed Picos.Computation.finished

let schedule_micro_task (self : t) (f : unit -> unit) : unit =
  let task = T { k = ES.fiber f; arg = (); comp = basic_comp_ } in
  Queue.push task self.cur_q

let[@inline] schedule_ (self : t) (task : task) : unit =
  Queue.push task self.next_q

let[@inline] active self = Atomic.get self.active
let[@inline] n_tasks self = self.n_tasks

let[@inline] has_tasks self =
  not (Queue.is_empty self.cur_q && Queue.is_empty self.next_q)

let create () : t =
  {
    active = Atomic.make true;
    cur_q = Queue.create ();
    next_q = Queue.create ();
    n_tasks = 0;
  }

(** Run a task. This might, via effects or [spawn],
    create some more tasks, and it might also suspend the current task. *)
let run_one_ (self : t) (task : task) : unit =
  let rec run : type a. (a, unit) ES.continuation -> a -> unit =
   fun cont x ->
    ES.continue_with cont x { ES.retc = Fun.id; exnc = raise; effc }
  and effc : type c. c Effect.t -> ((c, unit) ES.continuation -> unit) option =
    function
    | Picos.Trigger.Await trigger ->
      Some
        (fun cont' ->
          let on_trigger_signal_ _trigger () () : unit = run cont' () in

          let attached =
            Picos.Trigger.on_signal trigger () () on_trigger_signal_
          in
          if not attached then (
            let bt = Printexc.get_raw_backtrace () in
            ES.discontinue_with_backtrace cont' Sys.Break bt
              { ES.retc = Fun.id; exnc = raise; effc }
          ))
    | Picos.Fiber.Spawn { forbid = _; computation; mains } ->
      (* TODO: use [forbid] *)
      let schedule_fun_ f =
        let task =
          T
            {
              k = ES.fiber f;
              arg = ();
              comp = Picos.Computation.Packed computation;
            }
        in
        schedule_ self task
      in

      Some
        (fun cont' ->
          List.iter schedule_fun_ mains;
          ES.continue_with cont' () { ES.retc = Fun.id; exnc = raise; effc })
    | _ -> None
  in

  let (T { k; arg; comp = Packed comp }) = task in

  match Picos.Computation.canceled comp with
  | None -> run k arg
  | Some ebt ->
    ES.discontinue_with_backtrace k ebt.exn ebt.bt
      { ES.retc = Fun.id; exnc = raise; effc }

let[@inline] dispose (self : t) : unit = Atomic.set self.active false

let[@inline] as_disposable self =
  { Disposable.dispose = (fun () -> dispose self) }

let spawn (self : t) f : _ Fiber.t =
  let fib = Fiber.Private.create () in
  let comp = Fiber.Private.comp fib in

  let main () =
    try
      let res = f () in
      Picos.Computation.return comp res
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      Picos.Computation.cancel comp @@ { Exn_bt.exn; bt }
  in

  schedule_ self (T { k = ES.fiber main; arg = (); comp = Packed comp });
  fib

let run_iteration (self : t) : unit =
  Queue.transfer self.next_q self.cur_q;

  let continue = ref true in
  while !continue && active self do
    match Queue.pop self.cur_q with
    | exception Queue.Empty -> continue := false
    | task ->
      self.n_tasks <- 1 + self.n_tasks;
      run_one_ self task
  done
