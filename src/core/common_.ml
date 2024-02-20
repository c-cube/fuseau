module A = Atomic
module TLS = Thread_local_storage
module ES = Effect.Shallow
module Trace = Trace_core

let spf = Printf.sprintf
let ( let@ ) = ( @@ )

module Int_map = Map.Make (Int)
module Int_set = Set.Make (Int)

let _default_buf_size = 16 * 1024
