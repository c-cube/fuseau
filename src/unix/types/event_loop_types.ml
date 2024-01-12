type event = Fuseau_unix.Event_handle.t = { cancel: unit -> unit } [@@unboxed]

let stop_event (ev : event) = ev.cancel ()

type file_descr = Unix.file_descr
