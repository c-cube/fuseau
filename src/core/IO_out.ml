open Common_

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
