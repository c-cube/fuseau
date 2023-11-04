(** Byte buffers, using bigarrays. *)

include Luv.Buffer

module Private = struct
  let dummy : t = create 0
end
