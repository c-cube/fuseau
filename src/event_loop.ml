(** Event loop. *)

open Common_

type t = {
  loop: Luv.Loop.t;
  scheduler: Scheduler.t;
  mutable idle: Luv.Idle.t option;  (** Run user functions *)
  mutable check: Luv.Check.t option;
}

let[@inline] loop self = self.loop

(** Current loop, using TLS *)
module Current = struct
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

let run_idle_ (self : t) : unit =
  print_endline "run idle";
  Scheduler.run_iteration self.scheduler;
  print_endline "run idle done"

let[@inline never] create_idle_ (self : t) =
  if Option.is_none self.idle then (
    print_endline "add idle";
    let idle = Luv.Idle.init ~loop:self.loop () |> Err.unwrap_luv in

    self.idle <- Some idle;
    Luv.Idle.start idle (fun () -> run_idle_ self) |> Err.unwrap_luv
  )

let[@inline never] remove_idle_ (self : t) idle =
  print_endline "remove idle";
  self.idle <- None;
  Luv.Idle.stop idle |> Err.unwrap_luv

let run_check_ (self : t) =
  print_endline "run check";
  match self.idle with
  | Some idle ->
    if not (Scheduler.Private.has_pending_tasks self.scheduler) then
      remove_idle_ self idle
  | None ->
    if Scheduler.Private.has_pending_tasks self.scheduler then create_idle_ self

let spawn (self : t) (f : unit -> 'a) : ('a, _) Computation.t =
  (* TODO: also have a [Luv.Async.t] to wakeup the loop
     if needed *)
  let comp = Scheduler.spawn_from_anywhere self.scheduler f in
  assert (Scheduler.Private.has_pending_tasks self.scheduler);
  if Option.is_none self.idle then create_idle_ self;
  comp

let with_scheduler_ sched f =
  let r = TLS.get Scheduler.Private.k_current_scheduler in
  let old = !r in
  r := Some sched;
  Fun.protect ~finally:(fun () -> r := old) f

let create_uv_loop () : Luv.Loop.t = Luv.Loop.init () |> Err.unwrap_luv

let main ?(loop = Luv.Loop.default ()) (main : unit -> 'a) : 'a =
  let scheduler = Scheduler.create () in
  let self = { scheduler; loop; idle = None; check = None } in

  (* setup check: after running our code, checking on IOs, see if
     we need an [idle] for next iteration (i.e do we have tasks
     readily available to run) *)
  let check = Luv.Check.init ~loop:self.loop () |> Err.unwrap_luv in
  Luv.Check.start check (fun () -> run_check_ self) |> Err.unwrap_luv;
  self.check <- Some check;

  let@ () = Current.with_ self in

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

  let did_not_complete : bool =
    (* run with scheduler *)
    let@ () = with_scheduler_ self.scheduler in
    Luv.Loop.run ~loop ()
  in
  ignore did_not_complete;

  Option.iter (fun c -> Luv.Check.stop c |> Err.unwrap_luv) self.check;

  assert (Computation.is_done _get_res);

  match !res with
  | None -> assert false
  | Some (Ok x) -> x
  | Some (Error ebt) -> Exn_bt.raise ebt

module Private = struct
  let get_current_exn = Current.get_exn
end
