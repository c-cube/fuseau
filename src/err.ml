type t =
  | E_luv of Luv.Error.t  (** Luv error *)
  | E_not_in_main  (** Operation needs to be run in main *)

exception E of t

let unwrap_luv = function
  | Ok x -> x
  | Error e -> raise (E (E_luv e))
