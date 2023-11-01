(** Wall clock time in seconds *)
let now_s () : float =
  let t = Luv.Time.gettimeofday () |> Err.unwrap_luv in
  Int64.to_float t.tv_sec +. (Int32.to_float t.tv_usec *. 1e-6)

(** Wall clock time in microseconds *)
let now_us () : int64 =
  let t = Luv.Time.gettimeofday () |> Err.unwrap_luv in
  Int64.(add (mul t.tv_sec (of_int 1_000_000)) (of_int32 t.tv_usec))

(** Time in nanoseconds *)
let monotonic_time_ns () : Unsigned.uint64 = Luv.Time.hrtime ()

let monotonic_time_s () : float =
  let ns = Luv.Time.hrtime () in
  let us = Unsigned.UInt64.(div ns (of_int 1000) |> to_int64) in
  Int64.to_float us *. 1e-6
