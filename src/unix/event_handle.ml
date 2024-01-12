type t = { cancel: unit -> unit } [@@unboxed]

let[@inline] cancel self = self.cancel ()
