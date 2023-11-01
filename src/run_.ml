open Common_
module E = Effect
module ES = Effect.Shallow

let rec run_in_idle ~loop (f : unit -> unit) : unit =
  let idle = Luv.Idle.init ~loop () |> Err.unwrap_luv in
  let cleanup () = ignore (Luv.Idle.stop idle : _ result) in

  let c0 = ES.fiber f in

  let rec run : type a. (a, unit) ES.continuation -> a -> unit =
   fun cont x ->
    ES.continue_with cont x { ES.retc = Fun.id; exnc = raise; effc }
  and effc : type c. c E.t -> ((c, unit) ES.continuation -> unit) option =
    function
    | Effects_.Suspend { suspended } ->
      Some
        (fun cont' ->
          let wakeup () = run cont' () in
          suspended ~wakeup)
    | Effects_.Spawn f ->
      Some
        (fun cont' ->
          run_in_idle ~loop f;
          ES.continue_with cont' () { ES.retc = Fun.id; exnc = raise; effc })
    | _ -> None
  in

  Luv.Idle.start idle (fun () ->
      let@ () = Fun.protect ~finally:cleanup in
      run c0 ())
  |> Err.unwrap_luv

let run_in_idle_fut ~loop (f : unit -> 'a) : 'a Fut.t =
  let fut, promise = Fut.make () in
  run_in_idle ~loop (fun () ->
      try
        let x = f () in
        Fut.fulfill promise @@ Ok x
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Fut.fulfill_idempotent promise @@ Error { Picos.Exn_bt.exn; bt });
  fut
