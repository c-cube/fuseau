open Common_

type t = {
  sched: Scheduler.t;
  ev_loop: Event_loop.t;
}

let with_scheduler_ (sched : Scheduler.t) f =
  let r = TLS.get Scheduler.Internal_.k_current_scheduler in
  let old = !r in
  r := Some sched;
  Fun.protect ~finally:(fun () -> r := old) f

let main_loop_ (self : t) : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "fuseau.main" in
  let continue = ref true in
  while !continue do
    let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "fuseau.loop.step" in
    Scheduler.run_iteration self.sched;

    (* run a step of libuv polling, but without waiting because
       we might get new fibers to run *)
    Event_loop.one_step self.ev_loop ~block:false ();

    let sched_active = Scheduler.Internal_.has_pending_tasks self.sched in
    let ev_loop_active = Event_loop.has_pending_tasks self.ev_loop in
    match sched_active, ev_loop_active with
    | true, _ -> () (* continue *)
    | false, true ->
      (* run a step of libuv polling + waiting *)

      (* TODO:
         (* create an async to allow external [spawn] to wake us up *)
         let async = Luv.Async.init ~loop:self.loop ignore |> Err.unwrap_luv in
         self.async <- Some async;
      *)
      self.ev_loop#one_step ~block:true ()
      (* TODO:
         (* cleanup async *)
         Luv.Handle.close async ignore;
         self.async <- None;
         Atomic.set self.did_trigger_async false
      *)
    | false, false ->
      (* no more work to do, exit *)
      continue := false
  done

let main ~loop:ev_loop (main : unit -> 'a) : 'a =
  let sched = Scheduler.create ~ev_loop () in
  let self = { sched; ev_loop } in

  (* run the loop that interleaves scheduler and Libuv steps *)
  let fiber =
    let@ () = with_scheduler_ self.sched in

    (* run main task *)
    let fib =
      Scheduler.spawn ~name:"fuseau_main" ~propagate_cancel_to_parent:true main
    in

    main_loop_ self;
    fib
  in

  match Fiber.peek fiber with
  | Wait _ -> assert false (* should not have stopped *)
  | Done x -> x
  | Fail ebt -> Exn_bt.raise ebt
