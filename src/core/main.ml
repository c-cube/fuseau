open Common_

type t = {
  sched: Scheduler.t;
  ev_loop: Event_loop.t;
  mutable main_fiber: Fiber.any;
}

let with_scheduler_ (sched : Scheduler.t) f =
  let r = TLS.get Scheduler.k_current_scheduler in
  let old = !r in
  r := Some sched;
  Fun.protect ~finally:(fun () -> r := old) f

let main_loop_ (self : t) : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "fuseau.main" in
  let (Any_fiber main_fiber) = self.main_fiber in

  (* make sure to exit the main loop when the main fiber exits *)
  Fiber.on_res main_fiber (fun _ ->
      Event_loop.interrupt_if_in_blocking_section self.ev_loop);

  while not (Fiber.is_done main_fiber) do
    (* let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "fuseau.loop.step" in *)
    Trace.counter_int "fuseau.n-tasks" (Scheduler.n_queued_tasks self.sched);

    Scheduler.run_iteration self.sched;

    (* run a step of libuv polling, but without waiting because
       we might get new fibers to run *)
    Event_loop.one_step self.ev_loop ~block:false ();

    let sched_active = Scheduler.has_pending_tasks self.sched in

    if sched_active then
      (* continue *)
      ()
    else if Fiber.is_done main_fiber then
      ()
    else (
      Event_loop.one_step self.ev_loop ~block:true ();
      Trace.counter_int "fuseau.n-tasks" (Scheduler.n_queued_tasks self.sched)
    )
  done

let main ~loop:ev_loop (main : unit -> 'a) : 'a =
  let sched = Scheduler.create ~ev_loop () in
  let self =
    {
      sched;
      ev_loop;
      main_fiber = (* placeholder *) Fiber.Any_fiber (Fiber.return ());
    }
  in

  (* run the loop that interleaves scheduler and Libuv steps *)
  let fiber =
    let@ () = with_scheduler_ self.sched in

    (* run main task *)
    let fib =
      Scheduler.spawn ~name:"fuseau_main" ~propagate_cancel_to_parent:true main
    in
    self.main_fiber <- Fiber.Any_fiber fib;

    main_loop_ self;
    Trace.message "main loop done";
    fib
  in

  match Fiber.peek fiber with
  | None -> assert false (* should not have stopped *)
  | Some (Ok x) -> x
  | Some (Error ebt) -> Exn_bt.raise ebt
