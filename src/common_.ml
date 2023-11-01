module LH = Luv.Handle
module TLS = Thread_local_storage

let spf = Printf.sprintf
let ( let@ ) = ( @@ )

(** Key to access current loop *)
let k_cur_loop : Luv.Loop.t option ref TLS.key =
  TLS.new_key (fun () -> ref None)

let with_cur_loop loop f =
  let cur = TLS.get k_cur_loop in
  let old = !cur in
  let@ () = Fun.protect ~finally:(fun () -> cur := old) in
  cur := Some loop;
  f ()

let[@inline] get_cur_loop_exn () : Luv.Loop.t =
  let cur = TLS.get k_cur_loop in
  match !cur with
  | Some loop -> loop
  | None -> raise Err.(E E_not_in_main)
