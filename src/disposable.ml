(** Something that can be terminated *)

type t = { dispose: unit -> unit } [@@unboxed]

let[@inline] dispose (self : t) : unit = self.dispose ()
