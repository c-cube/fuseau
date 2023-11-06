(** Something that can be terminated *)

class type t =
  object
    method dispose : unit
  end

let[@inline] dispose (self : #t) : unit = self#dispose
