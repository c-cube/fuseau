open Common_

class type t =
  object
    method input : bytes -> int -> int -> int
    (** Read into the slice. Returns [0] only if the
        stream is closed. *)

    method close : unit -> unit
    (** Close the input. Must be idempotent. *)
  end

let create ?(close = ignore) ~input () : t =
  object
    method close = close
    method input = input
  end

let empty : t =
  object
    method close () = ()
    method input _ _ _ = 0
  end

let of_in_channel ?(close_noerr = false) (ic : in_channel) : t =
  object
    method input buf i len = input ic buf i len

    method close () =
      if close_noerr then
        close_in_noerr ic
      else
        close_in ic
  end

let open_file ?close_noerr ?(mode = 0o644)
    ?(flags = [ Open_rdonly; Open_binary ]) filename : t =
  let ic = open_in_gen flags mode filename in
  of_in_channel ?close_noerr ic

let with_open_file ?close_noerr ?mode ?flags filename f =
  let ic = open_file ?close_noerr ?mode ?flags filename in
  Fun.protect ~finally:ic#close (fun () -> f ic)

let of_bytes ?(off = 0) ?len (b : bytes) : t =
  (* i: current position in [b] *)
  let i = ref off in

  let len =
    match len with
    | Some n ->
      if n > Bytes.length b - off then invalid_arg "Iostream.In.of_bytes";
      n
    | None -> Bytes.length b - off
  in
  let end_ = off + len in

  object
    method input b_out i_out len_out =
      let n = min (end_ - !i) len_out in
      Bytes.blit b !i b_out i_out n;
      i := !i + n;
      n

    method close () = i := end_
  end

let of_string ?off ?len s : t = of_bytes ?off ?len (Bytes.unsafe_of_string s)

(** Read into the given slice.
      @return the number of bytes read, [0] means end of input. *)
let[@inline] input (self : #t) buf i len = self#input buf i len

(** Close the channel. *)
let[@inline] close self : unit = self#close ()

let rec really_input (self : #t) buf i len =
  if len > 0 then (
    let n = input self buf i len in
    if n = 0 then raise End_of_file;
    (really_input [@tailrec]) self buf (i + n) (len - n)
  )

let really_input_string self n : string =
  let buf = Bytes.create n in
  really_input self buf 0 n;
  Bytes.unsafe_to_string buf

let copy_into ?(buf = Bytes.create _default_buf_size) (ic : #t) (oc : IO_out.t)
    : unit =
  let continue = ref true in
  while !continue do
    let len = input ic buf 0 (Bytes.length buf) in
    if len = 0 then
      continue := false
    else
      IO_out.output oc buf 0 len
  done

let concat (l0 : t list) : t =
  let l = ref l0 in
  let rec input b i len : int =
    match !l with
    | [] -> 0
    | ic :: tl ->
      let n = ic#input b i len in
      if n > 0 then
        n
      else (
        l := tl;
        input b i len
      )
  in
  let close () = List.iter close l0 in
  create ~close ~input ()

let input_all ?(buf = Bytes.create 128) (self : #t) : string =
  let buf = ref buf in
  let i = ref 0 in

  let[@inline] full_ () = !i = Bytes.length !buf in

  let grow_ () =
    let old_size = Bytes.length !buf in
    let new_size = min Sys.max_string_length (old_size + (old_size / 4) + 10) in
    if old_size = new_size then
      failwith "input_all: maximum input size exceeded";
    let new_buf = Bytes.extend !buf 0 (new_size - old_size) in
    buf := new_buf
  in

  let rec loop () =
    if full_ () then grow_ ();
    let available = Bytes.length !buf - !i in
    let n = input self !buf !i available in
    if n > 0 then (
      i := !i + n;
      (loop [@tailrec]) ()
    )
  in
  loop ();

  if full_ () then
    Bytes.unsafe_to_string !buf
  else
    Bytes.sub_string !buf 0 !i

let of_unix_fd ?(close_noerr = false) (fd : Unix.file_descr) : t =
  object
    method input buf i len = Unix.read fd buf i len

    method close () =
      if close_noerr then (
        try Unix.close fd with _ -> ()
      ) else
        Unix.close fd
  end
