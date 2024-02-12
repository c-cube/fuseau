module F = Fuseau_lwt
module Trace = Trace_core
module Str_set = CCSet.Make (String)

module Uri_tbl = CCHashtbl.Make (struct
  include Uri

  let hash u = Hashtbl.hash (to_string u)
end)

let ( let@ ) = ( @@ )
let spf = Printf.sprintf
let verbose_ = ref 0

module Run = struct
  type t = {
    domains: Str_set.t; (* domains to recursively crawl *)
    mutable in_flight: int;
    pool: Moonpool.Runner.t;
    tasks: Uri.t F.Chan.t;
    default_host: string;
    max: int;
    seen: unit Uri_tbl.t; (* already explored *)
    mutable bad: Uri.t list;
    mutable n: int; (* number of pages crawled *)
    j: int;
    w: int;
  }

  let push_task (self : t) u : unit =
    let u = Uri.canonicalize u in
    if not @@ Uri_tbl.mem self.seen u then (
      Uri_tbl.add self.seen u ();
      try F.Chan.send self.tasks u with F.Chan.Closed -> ()
    )

  let make ~j ~w ~domains ~default_host ~max start : t =
    (* include the domains of [start] in [domains] *)
    let domains =
      List.fold_left
        (fun set uri ->
          match Uri.host uri with
          | None -> set
          | Some h -> Str_set.add h set)
        domains start
    in
    let j =
      if j = 0 then
        Moonpool.recommended_thread_count ()
      else
        j
    in
    let pool = Moonpool.Ws_pool.create ~num_threads:j () in
    let self =
      {
        domains;
        j;
        w;
        max;
        in_flight = 0;
        pool;
        tasks = F.Chan.create ~max_size:10_000 ();
        default_host;
        seen = Uri_tbl.create 256;
        bad = [];
        n = 0;
      }
    in
    List.iter (fun uri -> push_task self uri) start;
    self

  let bad_code c = c >= 400

  let find_urls ~in_url (body : string) : Uri.t list =
    let@ _sp =
      Trace.with_span ~__FILE__ ~__LINE__ "argiope.find-urls" ~data:(fun () ->
          [ "in_url", `String in_url; "size", `Int (String.length body) ])
    in
    let body =
      (* make sure to limit the size *)
      let body =
        if String.length body > 100_000 then
          String.sub body 0 100_000
        else
          body
      in
      Soup.parse body
    in

    let open Soup.Infix in
    let nodes = body $$ "a[href]" in
    Soup.fold
      (fun l n ->
        try
          let url' = Soup.R.attribute "href" n in
          Uri.of_string url' :: l
        with _ -> l)
      [] nodes

  let check_if_done_ self =
    (*Printf.eprintf "CHECK: inflight=%d size=%d\n%!" self.in_flight
      (F.Chan.size self.tasks);*)
    if self.in_flight = 0 && F.Chan.is_empty self.tasks then
      F.Chan.close self.tasks

  let process_task (self : t) ~idx ~client (uri : Uri.t) : unit =
    self.in_flight <- 1 + self.in_flight;
    if !verbose_ > 0 then
      Printf.eprintf "[w%d] crawl %s\n%!" idx (Uri.to_string uri);

    (* fetch URL (only 100kb) *)
    self.n <- 1 + self.n;
    let resp =
      let fut =
        Ezcurl_lwt.get ~client ~range:"0-100000" ~url:(Uri.to_string uri) ()
      in
      let@ () = Fuseau.with_cancel_callback (fun _ -> Lwt.cancel fut) in

      F.await_lwt fut
    in

    (match resp with
    | Ok { Ezcurl_lwt.code; body; _ } ->
      if !verbose_ > 1 then
        Printf.eprintf "[w%d] got code=%d body=%dB from %s\n%!" idx code
          (String.length body) (Uri.to_string uri);
      if bad_code code then (
        if !verbose_ > 1 then
          Printf.eprintf "[w%d] bad code when fetching %s: %d\n%!" idx
            (Uri.to_string uri) code;
        self.bad <- uri :: self.bad (* bad URL! *)
      ) else (
        (* if !verbose_ then Printf.eprintf "body for %s:\n%s\n" (Uri.to_string uri) body; *)
        let cur_host = Uri.host_with_default ~default:self.default_host uri in

        (* compute URIs on the background pool *)
        let uris =
          Fuseau_moonpool.await_fut
          @@ Moonpool.Fut.spawn ~on:self.pool (fun () ->
                 find_urls ~in_url:(Uri.to_string uri) body)
        in
        List.iter
          (fun uri' ->
            match Uri.host uri' with
            | Some h when Str_set.mem h self.domains ->
              (* follow this link *)
              if !verbose_ > 1 then
                Printf.eprintf "[w%d] follow link to %s\n%!" idx
                  (Uri.to_string uri');
              push_task self uri'
            | Some _ -> ()
            | None ->
              (* relative URL, make it absolute *)
              let uri' = Uri.with_host uri' (Some cur_host) in
              let uri' = Uri.with_port uri' (Uri.port uri) in
              let uri' = Uri.with_scheme uri' (Uri.scheme uri) in
              if !verbose_ > 1 then
                Printf.eprintf "[w%d] follow link to %s\n%!" idx
                  (Uri.to_string uri');
              push_task self uri')
          uris
      )
    | Error (_, msg) ->
      if !verbose_ > 2 then
        Printf.eprintf "[w%d] error when fetching %s:\n  %s\n%!" idx
          (Uri.to_string uri) msg;
      (* bad URL! *)
      self.bad <- uri :: self.bad);
    self.in_flight <- self.in_flight - 1;
    check_if_done_ self

  let worker (self : t) ~(idx : int) : unit =
    let client = Ezcurl_lwt.make () in

    let continue = ref true in
    while !continue do
      if self.max >= 0 && self.n + self.in_flight > self.max then (
        F.Chan.close self.tasks;
        continue := false
      ) else (
        match F.Chan.receive_exn self.tasks with
        | exception F.Chan.Closed -> continue := false
        | uri ->
          (try process_task self ~idx ~client uri
           with e ->
             Printf.eprintf "w[%d]: uncaught exn %s\n%!" idx
               (Printexc.to_string e))
      )
    done;
    if !verbose_ > 0 then Printf.eprintf "[w%d] exiting…\n%!" idx;
    ()

  let run (self : t) : Uri.t list * int * int =
    Printf.printf "run %d jobs…\ndomain(s): [%s]\n%!" self.j
      (String.concat "," @@ Str_set.elements self.domains);
    let workers =
      CCList.init self.w (fun idx ->
          F.spawn ~name:(spf "worker%d" idx) (fun () ->
              try worker ~idx self
              with e ->
                Printf.eprintf "[w%d]: uncaught exn %s\n%!" idx
                  (Printexc.to_string e)))
    in

    (* wait for all workers to be done *)
    List.iteri
      (fun idx w ->
        if !verbose_ > 0 then Printf.eprintf "waiting for w%d…\n%!" idx;
        F.await w)
      workers;
    Printf.eprintf "done waiting\n%!";
    self.bad, self.n, F.Chan.size self.tasks
end

let help_str =
  {|A web crawler that can typically be found in Texas.

usage: argiope url [url*] [option*]
|}

let main () : int =
  let@ () = Trace_tef.with_setup () in
  Sys.catch_break true;
  let t0 = Unix.gettimeofday () in
  let domains = ref Str_set.empty in
  let start = ref [] in
  let j = ref 0 in
  let w = ref 20 in
  let max_ = ref ~-1 in
  let opts =
    [
      "-v", Arg.Unit (fun _ -> incr verbose_), " verbose";
      ( "--domain",
        Arg.String (fun s -> domains := Str_set.add s !domains),
        " include given domain" );
      ( "-d",
        Arg.String (fun s -> domains := Str_set.add s !domains),
        " alias to --domain" );
      "--max", Arg.Set_int max_, " max number of pages to explore";
      "-w", Arg.Set_int w, " number of workers (default 20)";
      "-j", Arg.Set_int j, " number of background threads";
    ]
    |> Arg.align
  in
  Arg.parse opts (CCList.Ref.push start) help_str;
  if !start = [] then (
    Arg.usage opts help_str;
    1
  ) else (
    let start = List.map Uri.of_string !start in
    let default_host =
      match Uri.host @@ List.hd start with
      | Some h -> h
      | _ -> failwith "need absolute URIs"
      | exception _ -> failwith "need absolute URIs"
    in
    let run_state =
      Run.make ~default_host ~j:!j ~w:!w ~domains:!domains ~max:!max_ start
    in
    (* crawl *)
    let bad, num, remaining = F.main (fun () -> Run.run run_state) in
    let elapsed_s = Unix.gettimeofday () -. t0 in

    if bad <> [] then (
      Printf.printf
        "ERROR: crawled %d pages in %.4fs, %d dead links (%d remaining)\n" num
        elapsed_s (List.length bad) remaining;
      List.iter
        (fun uri -> Printf.printf "  dead: %s\n" (Uri.to_string uri))
        bad;
      1
    ) else (
      Printf.printf "OK: crawled %d pages in %.4fs (remaining %d)\n" num
        elapsed_s remaining;
      0
    )
  )

let () = exit @@ main ()
