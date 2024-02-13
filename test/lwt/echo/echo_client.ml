module F = Fuseau_lwt
module Trace = Trace_core

let ( let@ ) = ( @@ )

let main ~port ~n ~n_conn () : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "main" in

  let remaining = Atomic.make n in
  let all_done = Atomic.make 0 in

  Printf.printf "connecting to port %d\n%!" port;
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in

  let rec run_task () =
    let n = Atomic.fetch_and_add remaining (-1) in
    let _task_sp =
      Trace.enter_manual_toplevel_span ~__FILE__ ~__LINE__ "run-task"
        ~data:(fun () -> [ "n", `Int n ])
    in

    if n > 0 then (
      ( (* let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "connect.client" in *)
        F.Net.TCP_client.with_connect addr
      @@ fun ic oc ->
        let buf = Bytes.create 32 in

        for _j = 1 to 100 do
          let _sp =
            Trace.enter_manual_sub_span ~parent:_task_sp ~__FILE__ ~__LINE__
              "write.loop" ~data:(fun () -> [ "iter", `Int _j ])
          in
          F.IO_out.output_string oc "hello";
          F.IO_out.flush oc;

          (* read back something *)
          let _n = F.IO_in.really_input ic buf in
          Trace.exit_manual_span _sp;
          F.yield ()
        done;
        Trace.message "closing connection…" );

      (* run another task *)
      let (_ : _ F.Fiber.t) = F.spawn ~name:"run-task" run_task in
      ()
    ) else (
      (* if we're the last to exit, resolve the promise *)
      let n_already_done = Atomic.fetch_and_add all_done 1 in
      if n_already_done = n_conn - 1 then Printf.printf "all done\n%!"
    );

    Trace.exit_manual_span _task_sp
  in

  (* start the first [n_conn] tasks *)
  for _i = 1 to n_conn do
    ignore (F.spawn ~name:"run-task" run_task : _ F.Fiber.t)
  done;

  (* exit when [fut_exit] is resolved *)
  Printf.printf "done with main\n%!";
  ()

let () =
  let@ () = Trace_tef.with_setup () in
  Trace.set_thread_name "main";
  let port = ref 1234 in
  let n_conn = ref 100 in
  let n = ref 50_000 in

  let opts =
    [
      "-p", Arg.Set_int port, " port";
      "-n", Arg.Set_int n, " total number of connections";
      "--n-conn", Arg.Set_int n_conn, " number of parallel connections";
    ]
    |> Arg.align
  in
  Arg.parse opts ignore "echo client";

  Lwt_engine.set @@ new Lwt_engine.libev ();
  F.main @@ main ~port:!port ~n:!n ~n_conn:!n_conn
