open Common_

type t = bytes Resource_pool.t

let _default_size = 16 * 1024
let clear_buf_ (c : bytes) = Bytes.fill c 0 (Bytes.length c) '\x00'

let create ?(buf_size = _default_size) ?(max_size = 64) () : t =
  Resource_pool.create ~clear:clear_buf_ ~max_size
    ~mk_item:(fun () -> Bytes.create buf_size)
    ()

let acquire = Resource_pool.acquire
let recycle = Resource_pool.recycle
let with_buf = Resource_pool.with_resource
