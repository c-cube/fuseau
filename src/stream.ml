(** Bidirectional byte streams *)

type 'kind t = 'kind Luv.Stream.t
type any = Stream : [ `Stream of _ ] Luv.Stream.t -> any [@@unboxed]
