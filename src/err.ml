type t =
  | E_luv of Luv.Error.t  (** Luv error *)
  | E_not_in_main  (** Operation needs to be run in main *)

exception E of t

let to_string (e : t) : string =
  match e with
  | E_luv err -> Luv.Error.strerror err
  | E_not_in_main -> "operation must be run from within the event loop"

let pp out (e : t) = Format.pp_print_string out @@ to_string e

let () =
  Printexc.register_printer (function
    | E e -> Some (to_string e)
    | _ -> None)

let unwrap_luv = function
  | Ok x -> x
  | Error e -> raise (E (E_luv e))
