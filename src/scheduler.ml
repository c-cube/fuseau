open Common_

type task =
  | T : ('a, unit) ES.continuation * 'a -> task
      (** A task, ie a continuation to run *)

type t = {
  active: bool Atomic.t;
  q: task Queue.t;
  mutable n_tasks: int;
}

let[@inline] schedule_ (self : t) (task : task) : unit = Queue.push task self.q

let[@inline] schedule_fun_ (self : t) f : unit =
  schedule_ self (T (ES.fiber f, ()))

let[@inline] active self = Atomic.get self.active
let[@inline] n_tasks self = self.n_tasks
let[@inline] has_tasks self = not (Queue.is_empty self.q)

let create () : t =
  { active = Atomic.make true; q = Queue.create (); n_tasks = 0 }

let run_one_ (self : t) (task : task) : unit =
  let rec run : type a. (a, unit) ES.continuation -> a -> unit =
   fun cont x ->
    ES.continue_with cont x { ES.retc = Fun.id; exnc = raise; effc }
  and effc : type c. c Effect.t -> ((c, unit) ES.continuation -> unit) option =
    function
    | Suspend.Private.Suspend { suspended } ->
      Some
        (fun cont' ->
          let wakeup () = run cont' () in
          suspended ~wakeup)
    | Fiber.Private.Spawn f ->
      Some
        (fun cont' ->
          schedule_fun_ self f;
          ES.continue_with cont' () { ES.retc = Fun.id; exnc = raise; effc })
    | _ -> None
  in

  let (T (cont, x)) = task in
  run cont x

let[@inline] dispose (self : t) : unit = Atomic.set self.active false

let[@inline] as_disposable self =
  { Disposable.dispose = (fun () -> dispose self) }

let spawn (self : t) f : _ Fiber.t =
  let fib, promise = Fiber.Private.create () in
  schedule_fun_ self (fun () ->
      try
        let res = f () in
        Fut.fulfill promise (Ok res)
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Fut.fulfill_idempotent promise @@ Error { Picos.Exn_bt.exn; bt });
  fib

let run_iteration (self : t) : unit =
  let continue = ref true in
  while !continue && active self do
    match Queue.pop self.q with
    | exception Queue.Empty -> continue := false
    | task ->
      self.n_tasks <- 1 + self.n_tasks;
      run_one_ self task
  done
