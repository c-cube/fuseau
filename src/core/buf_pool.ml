type t = Cstruct.t Resource_pool.t

let _default_size = 16 * 1024
let clear_buf_ (c : Cstruct.t) = Cstruct.memset c 0

let create ?(buf_size = _default_size) ?(max_size = 64) () : t =
  Resource_pool.create ~clear:clear_buf_ ~max_size
    ~mk_item:(fun () -> Cstruct.create buf_size)
    ()

let acquire = Resource_pool.acquire
let recycle = Resource_pool.recycle
let with_buf = Resource_pool.with_resource
