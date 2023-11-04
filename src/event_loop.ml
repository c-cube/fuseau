(** Event loop. *)

open Common_

type t = {
  loop: Luv.Loop.t;
  scheduler: Scheduler.t;
  mutable async: Luv.Async.t option;
  did_trigger_async: bool Atomic.t;
}

let[@inline] loop self = self.loop

(** Current loop, using TLS *)
module Current_ev_loop = struct
  (** Key to access current loop *)
  let key : t option ref TLS.key = TLS.new_key (fun () -> ref None)

  let with_ loop f =
    let cur = TLS.get key in
    let old = !cur in
    let@ () = Fun.protect ~finally:(fun () -> cur := old) in
    cur := Some loop;
    f ()

  let[@inline] get_exn () : t =
    let cur = TLS.get key in
    match !cur with
    | Some loop -> loop
    | None -> raise Err.(E E_not_in_main)
end

let run_scheduler_tick_ (self : t) : unit =
  Scheduler.run_iteration self.scheduler

let spawn (self : t) (f : unit -> 'a) : ('a, _) Computation.t =
  let comp = Scheduler.spawn_from_anywhere self.scheduler f in
  assert (Scheduler.Private.has_pending_tasks self.scheduler);

  (* maybe trigger async to wake up loop *)
  if not (Atomic.exchange self.did_trigger_async true) then
    Option.iter (fun a -> Luv.Async.send a |> Err.unwrap_luv) self.async;

  comp

let with_scheduler_ sched f =
  let r = TLS.get Scheduler.Private.k_current_scheduler in
  let old = !r in
  r := Some sched;
  Fun.protect ~finally:(fun () -> r := old) f

let create_uv_loop () : Luv.Loop.t = Luv.Loop.init () |> Err.unwrap_luv

exception Exit_main

let main_loop_ (self : t) : unit =
  try
    while true do
      run_scheduler_tick_ self;

      (* run a step of libuv polling, but without waiting because
         we might get new fibers to run *)
      ignore (Luv.Loop.run ~loop:self.loop ~mode:`NOWAIT () : bool);

      let sched_active = Scheduler.Private.has_pending_tasks self.scheduler in
      let luv_active = Luv.Loop.alive self.loop in
      match sched_active, luv_active with
      | true, _ ->
        (* next iteration will run the next scheduler tick *)
        ()
      | false, true ->
        (* run a step of libuv polling + waiting *)
        print_endline "run once!";

        (* create an async to allow external [spawn] to wake us up *)
        let async = Luv.Async.init ~loop:self.loop ignore |> Err.unwrap_luv in
        self.async <- Some async;
        ignore (Luv.Loop.run ~loop:self.loop ~mode:`ONCE () : bool);

        (* cleanup async *)
        Luv.Handle.close async ignore;
        self.async <- None;
        Atomic.set self.did_trigger_async false
      | false, false ->
        (* no more work to do, exit *)
        raise_notrace Exit_main
    done
  with Exit_main -> ()

let main ?loop (main : unit -> 'a) : 'a =
  let loop =
    match loop with
    | None -> Luv.Loop.init () |> Err.unwrap_luv
    | Some l -> l
  in

  let scheduler = Scheduler.create ~loop () in
  let did_trigger_async = Atomic.make false in
  let self = { scheduler; loop; async = None; did_trigger_async } in

  let@ () = Current_ev_loop.with_ self in

  let res = ref None in
  let comp = spawn self main in

  let _get_res =
    spawn self (fun () ->
        match Computation.await comp with
        | r -> res := Some (Ok r)
        | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          res := Some (Error { Exn_bt.exn; bt }))
  in

  (* run the loop that interleaves scheduler and Libuv steps *)
  (let@ () = with_scheduler_ self.scheduler in
   main_loop_ self);
  assert (Computation.is_done _get_res);

  match !res with
  | None -> assert false
  | Some (Ok x) -> x
  | Some (Error ebt) -> Exn_bt.raise ebt

module Private = struct
  let get_current_exn = Current_ev_loop.get_exn
end
