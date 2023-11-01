module Private = struct
  type _ Effect.t +=
    | Suspend : { suspended: wakeup:(unit -> unit) -> unit } -> unit Effect.t
    [@@unboxed]
end

open Private

let[@inline] suspend f : unit = Effect.perform @@ Suspend { suspended = f }
