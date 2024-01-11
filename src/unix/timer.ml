open Common_

type time_ns = int64
type duration_ns = int64

type kind =
  | Once
  | Every of duration_ns

type task = {
  mutable deadline: time_ns;
  mutable active: bool;
  f: event_handle -> unit;
  as_event_handle: event_handle;
  kind: kind;
}

module Task_heap = Heap.Make (struct
  type t = task

  let[@inline] leq t1 t2 = t1.deadline <= t2.deadline
end)

type t = { mutable tasks: Task_heap.t }

(** accepted time diff for actions. *)
let epsilon_ns = 5_000L

type tick_res =
  | Wait of float
  | Run of (event_handle -> unit) * event_handle
  | Empty

let[@inline] has_tasks self = not (Task_heap.is_empty self.tasks)
let[@inline] num_tasks self : int = Task_heap.size self.tasks

let[@inline] pop_task_ self : unit =
  let tasks, _t = Task_heap.take_exn self.tasks in
  self.tasks <- tasks

let run_after self delay f : event_handle =
  let now = Time.monotonic_time_ns () in
  let deadline = Int64.(add now (mul (Int64.of_float delay) 1_000_000_000L)) in
  let rec task =
    {
      deadline;
      f;
      kind = Once;
      active = true;
      as_event_handle = { cancel = (fun () -> task.active <- false) };
    }
  in
  self.tasks <- Task_heap.insert task self.tasks;
  task.as_event_handle

let run_every self delay f : event_handle =
  let dur_ns = Int64.(mul (of_float delay) 1_000_000_000L) in
  let now = Time.monotonic_time_ns () in
  let deadline = Int64.(add now dur_ns) in
  let rec task =
    {
      deadline;
      f;
      kind = Every dur_ns;
      active = true;
      as_event_handle = { cancel = (fun () -> task.active <- false) };
    }
  in
  self.tasks <- Task_heap.insert task self.tasks;
  task.as_event_handle

let rec next (self : t) : tick_res =
  match Task_heap.find_min self.tasks with
  | None -> Empty
  | Some task when not task.active ->
    pop_task_ self;
    next self
  | Some task ->
    let now = Time.monotonic_time_ns () in

    let remaining_time = Int64.(sub task.deadline now) in
    if remaining_time <= epsilon_ns then (
      pop_task_ self;

      (match task.kind with
      | Once -> ()
      | Every dur ->
        (* schedule the next iteration *)
        task.deadline <- Int64.(add now dur);
        self.tasks <- Task_heap.insert task self.tasks);

      Run (task.f, task.as_event_handle)
    ) else (
      let remaining_time_s = Int64.to_float remaining_time *. 1e-9 in
      Wait remaining_time_s
    )

let create () = { tasks = Task_heap.empty }
