type 'a t = {
  fib: [ `Sync | `Async ] Picos.Fiber.t;
  comp: ('a, [ `Await | `Cancel | `Return ]) Picos.Computation.t;
}

module Private = struct
  let create () : 'a t =
    let comp = Picos.Computation.create () in
    let fib = Picos.Fiber.create ~forbid:false comp in
    { comp; fib }

  let[@inline] comp self :
      (_, [< `Await | `Cancel | `Return ]) Picos.Computation.t =
    self.comp
end

let[@inline] equal f1 f2 = f1 == f2
let[@inline] check self = Picos.Fiber.check self.fib
let[@inline] await (self : _ t) = Picos.Computation.await self.comp
let[@inline] is_done self = not (Picos.Computation.is_running self.comp)
let[@inline] forbid_cancellation self f = Picos.Fiber.forbid self.fib f
let[@inline] permit_cancellation self f = Picos.Fiber.permit self.fib f

let[@inline] cancel (self : _ t) reason : unit =
  Picos.Computation.cancel self.comp reason

module FLS = Picos.Fiber.FLS

let spawn (f : unit -> 'a) : 'a t =
  let self = Private.create () in

  let main () =
    try
      let res = f () in
      Picos.Computation.return self.comp res
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      Picos.Computation.cancel self.comp @@ { Exn_bt.exn; bt }
  in

  (* perform an effect to fulfill the promise *)
  Picos.Fiber.spawn ~forbid:false (self.comp :> _) [ main ];
  self
