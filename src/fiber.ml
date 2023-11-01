type 'a t = { res: 'a Fut.t } [@@unboxed]

let spawn f : _ t =
  let fut, promise = Fut.make () in

  Effect.perform
    (Effects_.Spawn
       (fun () ->
         try
           let res = f () in
           Fut.fulfill promise (Ok res)
         with exn ->
           let bt = Printexc.get_raw_backtrace () in
           Fut.fulfill_idempotent promise @@ Error { Picos.Exn_bt.exn; bt }));
  { res = fut }

let await (self : _ t) = Fut.await self.res
