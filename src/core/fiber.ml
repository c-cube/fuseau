open Common_
open Types

type 'a state = 'a fiber_status =
  | Done of 'a
  | Fail of Exn_bt.t
  | Wait of {
      waiters: 'a fiber_callback list;
      children: any_fiber FM.t;  (** Set of children *)
      on_cancel: cancel_callback list;
    }

type 'a t = 'a fiber
type 'a callback = 'a Types.fiber_callback
type cancel_callback = Exn_bt.t -> unit
type any = Types.any_fiber = Any_fiber : _ t -> any [@@unboxed]

let[@inline] peek self = A.get self.state

let[@inline] is_done self =
  match A.get self.state with
  | Done _ | Fail _ -> true
  | _ -> false

let[@inline] is_cancelled self =
  match A.get self.state with
  | Fail _ -> true
  | _ -> false

(** Register [f] to be called when the fiber ends.
    If the fiber is done already, call [f] immediately.
    [f] is called exactly once. *)
let on_res (self : _ t) f =
  while
    match A.get self.state with
    | Done x ->
      f (Ok x);
      false
    | Fail ebt ->
      f (Error ebt);
      false
    | Wait { waiters = l; children; on_cancel } as old ->
      not
        (A.compare_and_set self.state old
           (Wait { waiters = f :: l; children; on_cancel }))
  do
    ()
  done

(** Call waiters with [res] once all [children] are done *)
let call_waiters_once_children_are_done ~children ~waiters
    (res : 'a Exn_bt.result) : unit =
  let[@inline] call_waiters () =
    List.iter (fun (w : 'a fiber_callback) -> w res) waiters
  in

  let n_children = FM.cardinal children in
  if n_children > 0 then (
    (* wait for all children to be done *)
    let n_waiting = A.make (FM.cardinal children) in
    let on_child_finish (_ : _ result) =
      (* if we're the last to finish, wakeup the parent call *)
      if A.fetch_and_add n_waiting (-1) = 1 then call_waiters ()
    in
    FM.iter (fun _ (Any_fiber f) -> on_res f on_child_finish) children
  ) else
    call_waiters ()

(** Successfully resolve the fiber *)
let resolve (self : 'a t) (r : 'a) : unit =
  let new_st = Done r in
  while
    match A.get self.state with
    | Wait { waiters; children; on_cancel = _ } as old ->
      if A.compare_and_set self.state old new_st then (
        call_waiters_once_children_are_done ~children ~waiters (Ok r);
        false
      ) else
        true
    | Done _ | Fail _ -> false
  do
    ()
  done

let rec fail_fiber : type a. a t -> Exn_bt.t -> unit =
 fun self ebt ->
  (* Trace.messagef (fun k -> k "fail fiber[%d]" (self.id :> int)); *)
  let new_st = Fail ebt in
  while
    match A.get self.state with
    | Wait { waiters; children; on_cancel } as old ->
      if A.compare_and_set self.state old new_st then (
        (* here, unlike in {!resolve_fiber}, we immediately cancel children *)
        cancel_children ~children ebt;
        List.iter (fun cb -> cb ebt) on_cancel;
        call_waiters_once_children_are_done ~waiters ~children (Error ebt);
        false
      ) else
        true
    | Done _ | Fail _ -> false
  do
    ()
  done

(** Cancel eagerly all children *)
and cancel_children ebt ~children : unit =
  FM.iter (fun _ (Any_fiber f) -> fail_fiber f ebt) children

let remove_child (self : _ t) (child : _ t) =
  while
    match A.get self.state with
    | Wait { children; waiters; on_cancel } as old ->
      let new_st =
        Wait { children = FM.remove child.id children; waiters; on_cancel }
      in
      not (A.compare_and_set self.state old new_st)
    | _ -> false
  do
    ()
  done

(** Add a child to [self].
    @param protected if true, the child's failure will not affect [self]. *)
let add_child ~protected (self : _ fiber) (child : _ fiber) =
  while
    match A.get self.state with
    | Wait { children; waiters; on_cancel } as old ->
      let new_st =
        Wait
          {
            children = FM.add child.id (Any_fiber child) children;
            waiters;
            on_cancel;
          }
      in

      if A.compare_and_set self.state old new_st then (
        (* make sure to remove [child] from [self.children] once it's done *)
        on_res child (function
          | Ok _ -> remove_child self child
          | Error ebt ->
            remove_child self child;
            if not protected then fail_fiber self ebt);

        false
      ) else
        true
    | _ -> false
  do
    ()
  done

exception Cancelled of Exn_bt.t

module Internal_ = struct
  let create ?(name = "") () =
    let id = Fiber_handle.fresh () in
    {
      state =
        A.make @@ Wait { waiters = []; children = FM.empty; on_cancel = [] };
      id;
      name;
      fls = [||];
    }

  let resolve = resolve
  let cancel = fail_fiber
  let add_child = add_child
  let[@inline] cancel_any (Any_fiber f) ebt = cancel f ebt

  let[@inline] suspend ~before_suspend =
    Effect.perform @@ Effects.Suspend { before_suspend }

  (** A helper to get around circular dependencies. This is implemented via
      TLS, looking in the current thread's scheduler (if any). *)
  let get_current : (unit -> any option) ref = ref (fun () -> None)
end

let add_cancel_cb_ (self : _ t) cb =
  while
    match A.get self.state with
    | Wait { waiters; children; on_cancel } as old ->
      not
        (A.compare_and_set self.state old
           (Wait { waiters; children; on_cancel = cb :: on_cancel }))
    | Fail ebt ->
      cb ebt;
      false
    | Done _ -> false
  do
    ()
  done

let remove_top_cancel_cb_ (self : _ t) =
  while
    match A.get self.state with
    | Wait { on_cancel = []; _ } -> assert false
    | Wait { waiters; children; on_cancel = _ :: tl } as old ->
      not
        (A.compare_and_set self.state old
           (Wait { waiters; children; on_cancel = tl }))
    | Fail _ebt -> false
    | Done _ -> false
  do
    ()
  done

let with_cancel_callback cb (k : unit -> 'a) : 'a =
  match !Internal_.get_current () with
  | None -> failwith "with_cancel_callback` must be called from inside a fiber"
  | Some (Any_fiber f) ->
    add_cancel_cb_ f cb;
    Fun.protect k ~finally:(fun () -> remove_top_cancel_cb_ f)

let[@inline] get_exn_ self =
  match A.get self.state with
  | Done x -> x
  | Fail ebt -> Exn_bt.raise ebt
  | Wait _ ->
    Trace.message "fiber.getexn";
    assert false

let await self =
  match peek self with
  | Done x -> x
  | Fail ebt -> Exn_bt.raise ebt
  | Wait _ ->
    (* polling point *)
    (match !Internal_.get_current () with
    | None ->
      Trace.message "await outside of fiber";
      failwith "`await` must be called from inside a fiber"
    | Some (Any_fiber f) ->
      (match A.get f.state with
      | Fail ebt -> Exn_bt.raise ebt
      | _ -> ()));

    (* wait for resolution *)
    Effect.perform
    @@ Effects.Suspend
         { before_suspend = (fun ~wakeup -> on_res self (fun _ -> wakeup ())) };
    get_exn_ self

let try_await self =
  match peek self with
  | Done x -> Ok x
  | Fail ebt -> Error ebt
  | Wait _ ->
    (* polling point *)
    (match !Internal_.get_current () with
    | None -> failwith "`await` must be called from inside a fiber"
    | Some (Any_fiber f) ->
      (match A.get f.state with
      | Fail ebt -> Exn_bt.raise ebt
      | _ -> ()));

    (* wait for resolution *)
    Effect.perform
    @@ Effects.Suspend
         { before_suspend = (fun ~wakeup -> on_res self (fun _ -> wakeup ())) };

    (match A.get self.state with
    | Done x -> Ok x
    | Fail ebt -> Error ebt
    | Wait _ -> assert false)

let yield () =
  (* polling point *)
  (match !Internal_.get_current () with
  | None -> failwith "yield` must be called from inside a fiber"
  | Some (Any_fiber f) ->
    (match A.get f.state with
    | Fail ebt -> Exn_bt.raise ebt
    | Done _ -> assert false (* computation is still running *)
    | Wait _ -> ()));

  Effect.perform Effects.Yield
