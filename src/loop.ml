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
  let idle = Luv.Idle.init ~loop:self.loop () |> Err.unwrap_luv in

  Luv.Idle.start idle (fun () -> run_idle_ self) |> Err.unwrap_luv;
  self.idle <- Some idle;
  ()

let run_check_ (self : t) =
  print_endline "run check";
  match self.idle with
  | Some i ->
    if not (Scheduler.has_tasks self.scheduler) then (
      self.idle <- None;
      Luv.Idle.stop i |> Err.unwrap_luv
    )
  | None -> if Scheduler.has_tasks self.scheduler then create_idle_ self

let spawn (self : t) (f : unit -> 'a) : 'a Fiber.t =
  let fib = Scheduler.spawn self.scheduler f in
  run_check_ self;
  fib

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
  let fiber = spawn self main in
  let _get_res =
    spawn self (fun () ->
        match Fiber.await fiber with
        | r -> res := Some (Ok r)
        | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          res := Some (Error { Exn_bt.exn; bt }))
  in

  let did_not_complete : bool = Luv.Loop.run ~loop () in
  ignore did_not_complete;

  Option.iter (fun c -> Luv.Check.stop c |> Err.unwrap_luv) self.check;

  assert (Fiber.is_done _get_res);

  match !res with
  | None -> assert false
  | Some (Ok x) -> x
  | Some (Error ebt) -> Exn_bt.raise ebt
