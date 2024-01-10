(** Internal utilities, not exposed to the user *)

open Common_
open Types
module FM = Fiber_handle.Map

let on_res_fiber (self : _ fiber) f =
  while
    match A.get self.state with
    | Done x ->
      f (Ok x);
      false
    | Fail ebt ->
      f (Error ebt);
      false
    | Wait { waiters = l } as old ->
      not (A.compare_and_set self.state old (Wait { waiters = f :: l }))
  do
    ()
  done

let resolve_fiber (self : _ fiber) r : unit =
  let new_st = Done r in
  let cbs = ref [] in
  while
    match A.get self.state with
    | Wait { waiters = l } as old ->
      if A.compare_and_set self.state old new_st then (
        cbs := l;
        false
      ) else
        true
    | Done _ | Fail _ -> false
  do
    ()
  done;
  List.iter (fun f -> f (Ok r)) !cbs

let[@inline] perform_suspend ~before_suspend =
  Effect.perform @@ Effects.Suspend { before_suspend }

let rec cancel_switch (self : switch) ebt : unit =
  let new_st = Cancelled ebt in
  while
    match A.get self.state with
    | Active { children } as old ->
      if A.compare_and_set self.state old new_st then (
        (* cancel children *)
        cancel_children_and_wait ~children ebt;

        (* cancel parent if needed *)
        (match self.parent with
        | Some p when self.propagate_cancel_to_parent -> cancel_switch p ebt
        | _ -> ());

        false
      ) else
        true
    | _ -> false
  do
    ()
  done

and cancel_children_and_wait ebt ~children : unit =
  let n_children = FM.cardinal children in

  if n_children > 0 then (
    let n_waiting = A.make (FM.cardinal children) in
    perform_suspend ~before_suspend:(fun ~wakeup ->
        (* wait for all children to be done *)
        let on_child_finish (_ : _ result) =
          (* if we're the last to finish, wakeup the parent call *)
          if A.fetch_and_add n_waiting (-1) = 1 then wakeup ()
        in

        FM.iter (fun _ (Any_fiber f) -> cancel_switch f.switch ebt) children;
        FM.iter (fun _ (Any_fiber f) -> on_res_fiber f on_child_finish) children);

    ()
  )

let fail_fiber (self : _ fiber) ebt : unit =
  let new_st = Fail ebt in
  let cbs = ref [] in
  let do_cancel = ref false in

  while
    match A.get self.state with
    | Wait { waiters = l } as old ->
      if A.compare_and_set self.state old new_st then (
        cbs := l;
        do_cancel := true;
        false
      ) else
        true
    | Done _ | Fail _ -> false
  do
    ()
  done;

  if !do_cancel then (
    List.iter (fun f -> f (Error ebt)) !cbs;
    cancel_switch self.switch ebt
  )

let add_child_to_switch (self : switch) (Any_fiber f as af) =
  while
    match A.get self.state with
    | Active { children } as old ->
      let new_st = Active { children = FM.add f.id af children } in
      not (A.compare_and_set self.state old new_st)
    | _ -> false
  do
    ()
  done

let remove_child_from_switch (self : switch) (Any_fiber f) =
  while
    match A.get self.state with
    | Active { children } as old ->
      let new_st = Active { children = FM.remove f.id children } in
      not (A.compare_and_set self.state old new_st)
    | _ -> false
  do
    ()
  done
