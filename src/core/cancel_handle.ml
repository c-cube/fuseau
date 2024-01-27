(** Cancelation handle. *)

type t = { cancel: unit -> unit } [@@unboxed]
(** A handle to cancel atomic actions (waiting on something) *)

let[@inline] cancel self = self.cancel ()
let dummy : t = { cancel = ignore }
