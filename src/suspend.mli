val suspend : (wakeup:(unit -> unit) -> unit) -> unit

module Private : sig
  type _ Effect.t +=
    | Suspend : { suspended: wakeup:(unit -> unit) -> unit } -> unit Effect.t
    [@@unboxed]
end
