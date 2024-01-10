open Common_

type t = int

let counter_ = A.make 0
let fresh () = A.fetch_and_add counter_ 1
let equal : t -> t -> bool = ( = )
let compare : t -> t -> int = Stdlib.compare

(* TODO: better hash *)
let[@inline] hash x = x land max_int

module Set = Int_set
module Map = Int_map
