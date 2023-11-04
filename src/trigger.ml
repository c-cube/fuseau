include Picos.Trigger

let[@inline] await_or_raise (self : _ t) =
  match await self with
  | None -> ()
  | Some ebt -> Exn_bt.raise ebt
