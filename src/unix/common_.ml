module Trace = Trace_core
module TLS = Fuseau.Private_.TLS

let ( let@ ) = ( @@ )
let spf = Printf.sprintf

type cancel_handle = Cancel_handle.t = { cancel: unit -> unit } [@@unboxed]
