open Common_
open Types

type 'a state = 'a fiber_status =
  | Done of 'a Exn_bt.result
  | Wait of {
      already_done: 'a Exn_bt.result option;
      waiters: 'a fiber_callback list;
      children: any_fiber FM.t;  (** Set of children *)
      on_cancel: cancel_callback list;
    }

type 'a t = 'a fiber
type 'a callback = 'a Types.fiber_callback
type cancel_callback = Exn_bt.t -> unit
type any = Types.any_fiber = Any_fiber : _ t -> any [@@unboxed]

let[@inline] peek self =
  match A.get self.state with
  | Done x -> Some x
  | Wait _ -> None

let[@inline] is_done self =
  match A.get self.state with
  | Done _ -> true
  | _ -> false

let[@inline] is_cancelled self =
  match A.get self.state with
  | Done (Error _) -> true
  | _ -> false

(** Register [f] to be called when the fiber ends.
    If the fiber is done already, call [f] immediately.
    [f] is called exactly once. *)
let on_res (self : _ t) f =
  while
    match A.get self.state with
    | Done x ->
      f x;
      false
    | Wait ({ waiters = l; _ } as wait) as old ->
      not
        (A.compare_and_set self.state old (Wait { wait with waiters = f :: l }))
  do
    ()
  done

let fail_waiting_fiber (self : _ t) (ebt : Exn_bt.t) =
  while
    match A.get self.state with
    | Wait ({ already_done = Some (Ok _); _ } as wait) as old_st ->
      (* turn an about-to-succeed fiber to an about-to-fail one *)
      let new_st = Wait { wait with already_done = Some (Error ebt) } in
      not (A.compare_and_set self.state old_st new_st)
    | _ -> false
  do
    ()
  done

let resolve_to_final_state_and_call_waiters (self : _ t) : unit =
  (* get the final result *)
  match A.get self.state with
  | Wait { already_done = Some (Ok _ as res); waiters; _ } ->
    A.set self.state (Done res);
    List.iter (fun (w : 'a fiber_callback) -> w res) waiters
  | Wait { already_done = Some (Error ebt as res); waiters; on_cancel; _ } ->
    A.set self.state (Done res);
    (* also call cancel CBs *)
    List.iter (fun f -> f ebt) on_cancel;
    List.iter (fun (w : 'a fiber_callback) -> w res) waiters
  | Done _ | Wait { already_done = None; _ } -> assert false

(** Call waiters with [res] once all [children] are done *)
let call_waiters_and_set_res_once_children_are_done ~children (self : _ t) :
    unit =
  let n_children = FM.cardinal children in
  if n_children > 0 then (
    (* wait for all children to be done *)
    let n_waiting = A.make (FM.cardinal children) in

    let on_child_finish (res : _ result) =
      (match res with
      | Error ebt -> fail_waiting_fiber self ebt
      | Ok _ -> ());

      (* if we're the last to finish, resolve the fiber *)
      if A.fetch_and_add n_waiting (-1) = 1 then
        resolve_to_final_state_and_call_waiters self
    in

    FM.iter (fun _ (Any_fiber f) -> on_res f on_child_finish) children
  ) else
    (* no children, can resolve immediately *)
    resolve_to_final_state_and_call_waiters self

(** Successfully resolve the fiber *)
let resolve (self : 'a t) (r : 'a) : unit =
  while
    match A.get self.state with
    | Wait ({ children; already_done = None; _ } as wait) as old ->
      (* switch to [already_done=Some r] *)
      let new_st = Wait { wait with already_done = Some (Ok r) } in
      if A.compare_and_set self.state old new_st then (
        call_waiters_and_set_res_once_children_are_done ~children self;
        false
      ) else
        true
    | Wait { already_done = Some _; _ } | Done _ -> false
  do
    ()
  done

let rec fail_fiber : type a. a t -> Exn_bt.t -> unit =
 fun self ebt ->
  (* Trace.messagef (fun k -> k "fail fiber[%d]" (self.id :> int)); *)
  while
    match A.get self.state with
    | Wait ({ children; already_done = None; on_cancel; _ } as wait) as old ->
      let new_st =
        Wait { wait with already_done = Some (Error ebt); on_cancel = [] }
      in
      if A.compare_and_set self.state old new_st then (
        (* here, unlike in {!resolve_fiber}, we immediately cancel children *)
        cancel_children ~children ebt;
        List.iter (fun cb -> cb ebt) on_cancel;
        call_waiters_and_set_res_once_children_are_done ~children self;
        false
      ) else
        true
    | Wait { already_done = Some _; _ } | Done _ -> false
  do
    ()
  done

(** Cancel eagerly all children *)
and cancel_children ebt ~children : unit =
  FM.iter (fun _ (Any_fiber f) -> fail_fiber f ebt) children

let remove_child (self : _ t) (child : _ t) =
  while
    match A.get self.state with
    | Wait ({ children; _ } as wait) as old ->
      let new_st = Wait { wait with children = FM.remove child.id children } in
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
    | Wait ({ children; already_done = None; _ } as wait) as old ->
      let new_st =
        Wait { wait with children = FM.add child.id (Any_fiber child) children }
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

let create ?(name = "") () =
  let id = Fiber_handle.fresh () in
  {
    state =
      A.make
      @@ Wait
           {
             waiters = [];
             already_done = None;
             children = FM.empty;
             on_cancel = [];
           };
    id;
    name;
    fls = [||];
  }

let[@inline] return x : _ t =
  let id = Fiber_handle.fresh () in
  { id; fls = [||]; name = ""; state = A.make (Done (Ok x)) }

let[@inline] fail ebt : _ t =
  let id = Fiber_handle.fresh () in
  { id; fls = [||]; name = ""; state = A.make (Done (Error ebt)) }

let resolve = resolve
let cancel = fail_fiber
let add_child = add_child
let[@inline] cancel_any (Any_fiber f) ebt = cancel f ebt

let[@inline] suspend ~before_suspend =
  Effect.perform @@ Effects.Suspend { before_suspend }

(** A helper to get around circular dependencies. This is implemented via
      TLS, looking in the current thread's scheduler (if any). *)
let get_current : (unit -> any option) ref = ref (fun () -> None)

let add_cancel_cb_ (self : _ t) cb =
  while
    match A.get self.state with
    | Wait ({ on_cancel; _ } as wait) as old ->
      not
        (A.compare_and_set self.state old
           (Wait { wait with on_cancel = cb :: on_cancel }))
    | Done (Error ebt) ->
      cb ebt;
      false
    | Done (Ok _) -> false
  do
    ()
  done

let remove_top_cancel_cb_ (self : _ t) =
  while
    match A.get self.state with
    | Wait { on_cancel = []; _ } -> assert false
    | Wait ({ on_cancel = _ :: tl; _ } as wait) as old ->
      not (A.compare_and_set self.state old (Wait { wait with on_cancel = tl }))
    | Done (Error _ebt) -> false
    | Done (Ok _) -> false
  do
    ()
  done

let with_cancel_callback cb (k : unit -> 'a) : 'a =
  match !get_current () with
  | None -> failwith "with_cancel_callback` must be called from inside a fiber"
  | Some (Any_fiber f) ->
    add_cancel_cb_ f cb;
    Fun.protect k ~finally:(fun () -> remove_top_cancel_cb_ f)

let[@inline] get_exn_ self =
  match A.get self.state with
  | Done (Ok x) -> x
  | Done (Error ebt) -> Exn_bt.raise ebt
  | Wait _ ->
    Trace.message "fiber.getexn";
    assert false

let await self =
  match A.get self.state with
  | Done (Ok x) -> x
  | Done (Error ebt) -> Exn_bt.raise ebt
  | Wait _ ->
    (* polling point *)
    (match !get_current () with
    | None ->
      Trace.message "await outside of fiber";
      failwith "`await` must be called from inside a fiber"
    | Some (Any_fiber f) ->
      (match A.get f.state with
      | Done (Error ebt) -> Exn_bt.raise ebt
      | _ -> ()));

    (* wait for resolution *)
    suspend ~before_suspend:(fun ~wakeup -> on_res self (fun _ -> wakeup ()));
    get_exn_ self

let try_await self =
  match A.get self.state with
  | Done res -> res
  | Wait _ ->
    (* polling point *)
    (match !get_current () with
    | None -> failwith "`await` must be called from inside a fiber"
    | Some (Any_fiber f) ->
      (match A.get f.state with
      | Done (Error ebt) -> Exn_bt.raise ebt
      | _ -> ()));

    (* wait for resolution *)
    Effect.perform
    @@ Effects.Suspend
         { before_suspend = (fun ~wakeup -> on_res self (fun _ -> wakeup ())) };

    (match A.get self.state with
    | Done res -> res
    | Wait _ -> assert false)

let yield () =
  (* polling point *)
  (match !get_current () with
  | None -> failwith "yield` must be called from inside a fiber"
  | Some (Any_fiber f) ->
    (match A.get f.state with
    | Done (Error ebt) -> Exn_bt.raise ebt
    | Done _ -> assert false (* computation is still running *)
    | Wait _ -> ()));

  Effect.perform Effects.Yield

module Private_ = struct
  let create = create
  let cancel = cancel
end
