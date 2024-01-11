open Common_
open Types

type 'a state = 'a fiber_status =
  | Done of 'a
  | Fail of Exn_bt.t
  | Wait of { waiters: 'a fiber_callback list }

type 'a t = 'a fiber
type 'a callback = 'a Types.fiber_callback
type any = Types.any_fiber = Any_fiber : _ t -> any [@@unboxed]

let on_res = Fiber_switch_util_.on_res_fiber
let[@inline] peek self = A.get self.state
let[@inline] switch self = self.switch
let[@inline] switch_any (Any_fiber f) = f.switch

let is_cancelled self =
  match peek self with
  | Fail _ -> true
  | _ -> false

module Internal_ = struct
  let create ~switch () =
    let id = Fiber_handle.fresh () in
    { state = A.make @@ Wait { waiters = [] }; switch; id; fls = [||] }

  let resolve = Fiber_switch_util_.resolve_fiber
  let cancel = Fiber_switch_util_.fail_fiber
  let suspend = Fiber_switch_util_.perform_suspend

  (** A helper to get around circular dependencies. This is implemented via
      TLS, looking in the current thread's scheduler (if any). *)
  let get_current : (unit -> any option) ref = ref (fun () -> None)
end

let[@inline] get_exn_ self =
  match A.get self.state with
  | Done x -> x
  | Fail ebt -> Exn_bt.raise ebt
  | Wait _ -> assert false

let await self =
  match peek self with
  | Done x -> x
  | Fail ebt -> Exn_bt.raise ebt
  | Wait _ ->
    (* wait for resolution *)
    Effect.perform
    @@ Effects.Suspend
         { before_suspend = (fun ~wakeup -> on_res self (fun _ -> wakeup ())) };
    get_exn_ self
