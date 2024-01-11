let ( let@ ) = ( @@ )
let spf = Printf.sprintf

type event_handle = Event_loop.event_handle = { cancel: unit -> unit }
[@@unboxed]
