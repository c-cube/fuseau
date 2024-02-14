module F = Fuseau_unix

let last i1 i2 send_res =
  let count = ref 0. in
  let continue = ref true in
  while !continue do
    try
      let x =
        F.select
          [
            F.When (F.Chan.ev_receive i1, Fun.id);
            F.When (F.Chan.ev_receive i2, Fun.id);
          ]
      in
      (* Printf.printf "last: got %f\n%!" x; *)
      count := (!count *. 1.001) +. x
    with F.Chan.Closed -> continue := false
  done;

  let c = floor !count in
  Printf.printf "send final value %f\n%!" c;
  F.Chan.send send_res c

let forward _n i1 i2 o1 o2 =
  let continue = ref true in
  while !continue do
    match
      F.select
        [
          F.When (F.Chan.ev_receive i1, fun x -> x);
          F.When (F.Chan.ev_receive i2, fun x -> x);
        ]
    with
    | exception F.Chan.Closed ->
      (* Printf.printf "step %d: closing chan\n%!" _n; *)
      continue := false;
      F.Chan.close o1;
      F.Chan.close o2
    | x ->
      (* Printf.printf "step %d: forwarding %f\n%!" _n x; *)
      F.select
        [
          F.When (F.Chan.ev_send o1 x, ignore);
          F.When (F.Chan.ev_send o2 x, ignore);
        ]
  done

let rec mk_chain n i1 i2 send_res : unit =
  if n = 0 then
    ignore (F.spawn (fun () -> last i1 i2 send_res) : _ F.Fiber.t)
  else (
    let o1 = F.Chan.create ~max_size:4 () in
    let o2 = F.Chan.create ~max_size:4 () in

    ignore (F.spawn (fun () -> forward n i1 i2 o1 o2) : _ F.Fiber.t);
    ignore (F.spawn (fun () -> forward n i1 i2 o1 o2) : _ F.Fiber.t);
    mk_chain (n - 1) o1 o2 send_res
  )

let () =
  let n = 200 in
  let c1 = F.Chan.create ~max_size:4 () in
  let c2 = F.Chan.create ~max_size:4 () in

  let send_res = F.Chan.create ~max_size:10 () in

  let res =
    F.main (fun () ->
        Printf.printf "creating chain\n%!";
        mk_chain n c1 c2 send_res;

        for i = 1 to 1000 do
          if i mod 100 = 0 then Printf.printf "at iter %d\n%!" i;

          F.Chan.send c1 @@ float i;
          F.Chan.send c2 @@ float i
        done;
        F.Chan.close c1;
        F.Chan.close c2;

        let x = F.Chan.receive_exn send_res in
        x)
  in

  Printf.printf "res=%.0f\n%!" res
