module Fiber = Fiber
module Err = Err
module Timer = Timer
module Exn_bt = Exn_bt
module Loop = Loop

let create_loop () : Luv.Loop.t = Luv.Loop.init () |> Err.unwrap_luv
let main = Loop.main
