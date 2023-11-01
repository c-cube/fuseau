type 'a t = {
  id: int;
  res: 'a Fut.t;
}

module Private = struct
  type _ Effect.t +=
    | Spawn :
        (unit -> unit)
        -> unit Effect.t (* TODO: take bundle as arg, return fiber as result *)

  let counter_ : int Atomic.t = Atomic.make 0

  let create () : 'a t * 'a Fut.promise =
    let id = Atomic.fetch_and_add counter_ 1 in
    let fut, promise = Fut.make () in
    { id; res = fut }, promise
end

open Private

let[@inline] equal f1 f2 = f1.id = f2.id
let[@inline] res self = self.res
let[@inline] await (self : _ t) = Fut.await self.res

let spawn f : _ t =
  let fib, promise = Private.create () in
  (* perform an effect to fulfill the promise *)
  Effect.perform
    (Spawn
       (fun () ->
         try
           let res = f () in
           Fut.fulfill promise (Ok res)
         with exn ->
           let bt = Printexc.get_raw_backtrace () in
           Fut.fulfill_idempotent promise @@ Error { Picos.Exn_bt.exn; bt }));
  fib
