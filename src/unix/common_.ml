module Trace = Trace_core

let ( let@ ) = ( @@ )
let spf = Printf.sprintf

type cancel_handle = Cancel_handle.t = { cancel: unit -> unit } [@@unboxed]
