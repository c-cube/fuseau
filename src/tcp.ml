(** TCP streams *)

module Address_family = struct
  type t =
    [ `INET
    | `INET6
    | `OTHER of int
    | `UNSPEC
    ]

  (* same type *)
  let () = ignore (fun (x : t) : Sockaddr.Address_family.t -> x)
end

(** Connect to given address *)
let connect ?allocate ?domain (addr : Sockaddr.t) : Stream.ReadWritable.byte_t =
  let stream = Luv_tcp.init ?domain () in
  (* TODO: make it configurable *)
  Luv_tcp.nodelay stream true;
  Luv_tcp.connect stream addr;
  new Luv_stream.readable_writable_stream stream ?allocate

let establish_server ?domain ?ipv6only ?backlog (addr : Sockaddr.t)
    (handle_client : Luv_stream.readable_writable_stream -> unit) : Disposable.t
    =
  let stream = Luv_tcp.init ?domain () in
  (* TODO: make it configurable *)
  Luv_tcp.nodelay stream true;
  Luv_tcp.bind ?ipv6only stream addr;
  Luv_stream.listen ?backlog stream;

  let run () : unit =
    let fiber = Fiber.current () in
    let (Computation.Packed current_comp) = Fiber.computation fiber in

    while true do
      Fiber.check fiber;

      let sock_client = Luv_tcp.init ?domain () in

      Luv_stream.accept ~server:stream ~client:sock_client;
      Fiber.spawn ~forbid:false current_comp
        [
          (fun () ->
            let client = new Luv_stream.readable_writable_stream sock_client in
            handle_client client);
        ]
    done
  in

  let comp : (unit, _) Computation.t = Scheduler.spawn run in

  object
    method dispose =
      let bt = Printexc.get_callstack 10 in
      Computation.cancel comp { Exn_bt.exn = Sys.Break; bt }
  end
