open Common_

type t = {
  exn: exn;
  bt: Printexc.raw_backtrace;
}

let[@inline] make exn bt : t = { exn; bt }

let[@inline] get exn =
  let bt = Printexc.get_raw_backtrace () in
  { exn; bt }

let[@inline] get_callstack n exn =
  let bt = Printexc.get_callstack n in
  { bt; exn }

let show self = Printexc.to_string self.exn
let[@inline] raise self = Printexc.raise_with_backtrace self.exn self.bt

let[@inline] discontinue k self =
  ES.discontinue_with_backtrace k self.exn self.bt

let[@inline] discontinue_with k self h =
  ES.discontinue_with_backtrace k self.exn self.bt h

type nonrec 'a result = ('a, t) result
