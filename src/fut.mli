(** Futures.
*)

(* TODO: use picos *)

type 'a or_error = ('a, Picos.Exn_bt.t) result

type 'a t
(** A future with a result of type ['a]. *)

type 'a promise
(** A promise, which can be fulfilled exactly once to set
      the corresponding future *)

val make : unit -> 'a t * 'a promise
(** Make a new future with the associated promise *)

val on_result : 'a t -> ('a or_error -> unit) -> unit
(** [on_result fut f] registers [f] to be called in the future
      when [fut] is set ;
      or calls [f] immediately if [fut] is already set. *)

exception Already_fulfilled

val fulfill : 'a promise -> 'a or_error -> unit
(** Fullfill the promise, setting the future at the same time.
      @raise Already_fulfilled if the promise is already fulfilled. *)

val fulfill_idempotent : 'a promise -> 'a or_error -> unit
(** Fullfill the promise, setting the future at the same time.
      Does nothing if the promise is already fulfilled. *)

val return : 'a -> 'a t
(** Already settled future, with a result *)

val fail : exn -> Printexc.raw_backtrace -> _ t
(** Already settled future, with a failure *)

val of_result : 'a or_error -> 'a t

val is_resolved : _ t -> bool
(** [is_resolved fut] is [true] iff [fut] is resolved. *)

val peek : 'a t -> 'a or_error option
(** [peek fut] returns [Some r] if [fut] is currently resolved with [r],
      and [None] if [fut] is not resolved yet. *)

exception Not_ready

val get_or_fail : 'a t -> 'a or_error
(** [get_or_fail fut] obtains the result from [fut] if it's fulfilled
    (i.e. if [peek fut] returns [Some res], [get_or_fail fut] returns [res]).
    @raise Not_ready if the future is not ready. *)

val get_or_fail_exn : 'a t -> 'a

val is_done : _ t -> bool
(** Is the future resolved? This is the same as [peek fut |> Option.is_some]. *)

val await : 'a t -> 'a
