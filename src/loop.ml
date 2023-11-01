(** Event loop. *)

open Common_

type t = {
  loop: Luv.Loop.t;
  scheduler: Scheduler.t;
  mutable idle: Luv.Idle.t option;  (** Run user functions *)
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

let run_check_and_remove_ (self : t) c : unit =
  if not (Scheduler.has_tasks self.scheduler) then (
    (* disable idle so we may exit (unless some IOs are queued) *)
    Option.iter (fun i -> Luv.Idle.stop i |> Err.unwrap_luv) self.idle;
    self.idle <- None;

    Luv.Check.stop c |> Err.unwrap_luv
  )

let[@inline never] create_idle_ (self : t) =
  let idle = Luv.Idle.init ~loop:self.loop () |> Err.unwrap_luv in
  let check = Luv.Check.init ~loop:self.loop () |> Err.unwrap_luv in

  Luv.Idle.start idle (fun () ->
      run_check_and_remove_ self check;
      Scheduler.run_iteration self.scheduler)
  |> Err.unwrap_luv;
  self.idle <- Some idle;

  (* check if [idle] is still needed *)
  Luv.Idle.start idle (fun () -> run_check_and_remove_ self check)
  |> Err.unwrap_luv;
  ()

let[@inline] ensure_has_idle_ (self : t) =
  match self.idle with
  | Some _ -> ()
  | None -> create_idle_ self

let spawn (self : t) (f : unit -> 'a) : 'a Fiber.t =
  ensure_has_idle_ self;
  Scheduler.spawn self.scheduler f

let main ?(loop = Luv.Loop.default ()) (main : unit -> 'a) : 'a =
  let scheduler = Scheduler.create () in
  let self = { scheduler; loop; idle = None } in
  let@ () = Current.with_ self in

  let fiber = spawn self main in
  let did_not_complete : bool = Luv.Loop.run ~loop () in
  ignore did_not_complete;
  Fut.get_or_fail_exn @@ Fiber.res fiber
