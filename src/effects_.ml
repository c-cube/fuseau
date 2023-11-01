module E = Effect
module ES = Effect.Shallow

type _ E.t +=
  | Suspend : { suspended: wakeup:(unit -> unit) -> unit } -> unit E.t
  | Spawn :
      (unit -> unit)
      -> unit E.t (* TODO: take bundle as arg, return fiber as result *)
