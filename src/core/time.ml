(** Wall clock time in seconds *)
let now_s = Unix.gettimeofday

(** Monotonic time in nanoseconds *)
let monotonic_time_ns : unit -> int64 = Mtime_clock.now_ns

(** Monotonic time in seconds *)
let monotonic_time_s () : float =
  let ns = monotonic_time_ns () in
  let s = Int64.(div ns 1_000_000_000L) in
  let ns' = Int64.(rem ns 1_000_000_000L) in
  Int64.to_float s +. (Int64.to_float ns' /. 1e9)
