open Common_
open Types
module FM = Fiber_handle.Map

exception Cancel

type status = switch_status

type t = switch = {
  state: status A.t;
  parent: switch option;
  propagate_cancel_to_parent: bool;
}

let done_ : t =
  {
    state = A.make (Done : status);
    parent = None;
    propagate_cancel_to_parent = false;
  }

let create_root () : t =
  {
    state = A.make (Active { children = FM.empty });
    parent = None;
    propagate_cancel_to_parent = false;
  }

let create_sub ~parent ~propagate_cancel_to_parent () =
  {
    state = A.make (Active { children = FM.empty });
    parent = Some parent;
    propagate_cancel_to_parent;
  }

let cancel = Fiber_switch_util_.cancel_switch

module Internal_ = struct
  let add_child = Fiber_switch_util_.add_child_to_switch
  let remove_child = Fiber_switch_util_.remove_child_from_switch
end
