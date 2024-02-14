open Common_

module Out = struct
  class type t =
    object
      method output_char : char -> unit
      method output : bytes -> int -> int -> unit
      method flush : unit -> unit
      method close : unit -> unit
    end

  let create ?(flush = ignore) ?(close = ignore) ~output_char ~output () : t =
    object
      method flush () = flush ()
      method close () = close ()
      method output_char c = output_char c
      method output bs i len = output bs i len
    end

  let dummy : t =
    object
      method flush () = ()
      method close () = ()
      method output_char _ = ()
      method output _ _ _ = ()
    end

  let of_buffer (buf : Buffer.t) : t =
    object
      method close () = ()
      method flush () = ()
      method output_char c = Buffer.add_char buf c
      method output bs i len = Buffer.add_subbytes buf bs i len
    end

  (** Output the buffer slice into this channel *)
  let[@inline] output_char (self : #t) c : unit = self#output_char c

  (** Output the buffer slice into this channel *)
  let[@inline] output (self : #t) buf i len : unit = self#output buf i len

  let[@inline] output_string (self : #t) (str : string) : unit =
    self#output (Bytes.unsafe_of_string str) 0 (String.length str)

  let output_line (self : #t) (str : string) : unit =
    output_string self str;
    output_char self '\n'

  (** Close the channel. *)
  let[@inline] close self : unit = self#close ()

  (** Flush (ie. force write) any buffered bytes. *)
  let[@inline] flush self : unit = self#flush ()

  let output_int self i =
    let s = string_of_int i in
    output_string self s

  let output_lines self seq = Seq.iter (output_line self) seq

  let tee (l : t list) : t =
    match l with
    | [] -> dummy
    | [ oc ] -> oc
    | _ ->
      let output bs i len = List.iter (fun oc -> output oc bs i len) l in
      let output_char c = List.iter (fun oc -> output_char oc c) l in
      let close () = List.iter close l in
      let flush () = List.iter flush l in
      create ~flush ~close ~output ~output_char ()
end

module In = struct
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

  let copy_into ?(buf = Bytes.create _default_buf_size) (ic : #t) (oc : Out.t) :
      unit =
    let continue = ref true in
    while !continue do
      let len = input ic buf 0 (Bytes.length buf) in
      if len = 0 then
        continue := false
      else
        Out.output oc buf 0 len
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
      let new_size =
        min Sys.max_string_length (old_size + (old_size / 4) + 10)
      in
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
end
