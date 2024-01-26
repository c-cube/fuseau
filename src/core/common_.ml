module A = Atomic
module TLS = Thread_local_storage
module ED = Effect.Deep

let spf = Printf.sprintf
let ( let@ ) = ( @@ )

module Int_map = Map.Make (Int)
module Int_set = Set.Make (Int)

let _default_buf_size = 16 * 1024
