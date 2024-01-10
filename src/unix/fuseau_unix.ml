module Int_tbl = Hashtbl.Make (struct
  type t = int

  let equal : t -> t -> bool = ( = )
  let hash x = x land max_int
end)

type event_handle = Event_loop.event_handle = { cancel: unit -> unit }
[@@unboxed]

class ev_loop : Event_loop.t =
  object
    (* val read_ : (event_handle -> unit) Int_tbl.t = Int_tbl.create 32 *)
    method one_step ~block () : unit = assert false

    method on_readable
        : Unix.file_descr -> (event_handle -> unit) -> event_handle =
      assert false

    method on_writable
        : Unix.file_descr -> (event_handle -> unit) -> event_handle =
      assert false

    method on_timer
        : float -> repeat:bool -> (event_handle -> unit) -> event_handle =
      assert false
    (** [on_timer delay ~repeat f] runs [f] after [delay].
      @param repeat if true runs [f] every [delay] seconds *)

    method fake_io : Unix.file_descr -> unit = assert false
    (** Simulate activity on the FD *)

    method readable_count : int = assert false
    (** Number of events waiting for FDs to be readable FDs *)

    method writable_count : int = assert false

    method timer_count : int = assert false
    (** Number of events waiting on a timer *)
  end
