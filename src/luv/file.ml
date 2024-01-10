open Utils_
module Size_t = Unsigned.Size_t

type t = Luv.File.t

module Request = Luv.File.Request

let stdin = Luv.File.stdin
let stdout = Luv.File.stdout
let stderr = Luv.File.stderr

module Open_flag = Luv.File.Open_flag
module Mode = Luv.File.Mode

open struct
  let dummy_ : t = stderr

  let with_request () f =
    let req = Request.make () in
    let trigger = Trigger.create () in
    let cur_fiber = Fiber.current () in
    if not (Fiber.try_attach cur_fiber trigger) then
      Option.iter Exn_bt.raise (Fiber.canceled cur_fiber);

    let finally () = Fiber.detach cur_fiber trigger in
    let@ () = Fun.protect ~finally in
    f req

  let with_request_opt ?request () f =
    match request with
    | Some r -> f r
    | None -> with_request () f
end

(* TODO: always allocate a [request] and bind it to current fiber
   using [Picos.Fiber.try_attach]?
*)

let open_ ?request ?mode str (flags : Open_flag.t list) : t =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ dummy_ @@ fun k -> Luv.File.open_ ~loop ~request ?mode str flags k

let close ?request (self : t) : unit =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ () @@ fun k -> Luv.File.close ~loop ~request self k

let read ?request ?file_offset (self : t) (bufs : Buffer.t list) :
    Unsigned.Size_t.t =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ Size_t.zero @@ fun k ->
  Luv.File.read ~loop ~request ?file_offset self bufs k

let write ?request ?file_offset (self : t) bufs : Size_t.t =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ Size_t.zero @@ fun k ->
  Luv.File.write ~loop ~request ?file_offset self bufs k

let unlink ?request file : unit =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ () @@ fun k -> Luv.File.unlink ~loop ~request file k

let rename ?request file ~to_ : unit =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ () @@ fun k -> Luv.File.rename ~loop ~request file ~to_ k

let mkstemp ?request dir : string * t =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ ("", dummy_) @@ fun k -> Luv.File.mkstemp ~loop ~request dir k

let mkdtemp ?request dir : string =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ "" @@ fun k -> Luv.File.mkdtemp ~loop ~request dir k

let mkdir ?request ?mode dir : unit =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ () @@ fun k -> Luv.File.mkdir ~loop ~request ?mode dir k

let rmdir ?request dir : unit =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ () @@ fun k -> Luv.File.rmdir ~loop ~request dir k

module Dirent = Luv.File.Dirent
module Dir = Luv.File.Dir

let opendir ?request dir : Dir.t =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_noinit_ @@ fun k -> Luv.File.opendir ~loop ~request dir k

let closedir ?request (dir : Dir.t) : unit =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ () @@ fun k -> Luv.File.closedir ~loop ~request dir k

let readdir ?request ?number_of_entries (dir : Dir.t) : Dirent.t array =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ [||] @@ fun k ->
  Luv.File.readdir ~loop ~request ?number_of_entries dir k

module Directory_scan = Luv.File.Directory_scan

let scandir ?request dir : Directory_scan.t =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_noinit_ @@ fun k -> Luv.File.scandir ~loop ~request dir k

let scandir_next : Directory_scan.t -> Dirent.t option = Luv.File.scandir_next
let scandir_end : Directory_scan.t -> unit = Luv.File.scandir_end

module Stat = Luv.File.Stat

let stat ?request file : Stat.t =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_noinit_ @@ fun k -> Luv.File.stat ~loop ~request file k

let lstat ?request file : Stat.t =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_noinit_ @@ fun k -> Luv.File.lstat ~loop ~request file k

let fstat ?request file : Stat.t =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_noinit_ @@ fun k -> Luv.File.fstat ~loop ~request file k

module Statfs = Luv.File.Statfs

let statfs ?request file : Statfs.t =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_noinit_ @@ fun k -> Luv.File.statfs ~loop ~request file k

let fsync ?request (self : t) : unit =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ () @@ fun k -> Luv.File.fsync ~loop ~request self k

let fdatasync ?request (self : t) : unit =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ () @@ fun k -> Luv.File.fdatasync ~loop ~request self k

let ftruncate ?request (self : t) (len : int64) : unit =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ () @@ fun k -> Luv.File.ftruncate ~loop ~request self len k

let copyfile ?request ?excl ?ficlone ?ficlone_force (file : string) ~to_ : unit
    =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ () @@ fun k ->
  Luv.File.copyfile ~loop ~request ?excl ?ficlone ?ficlone_force file ~to_ k

let sendfile ?request (self : t) ~to_ ~offset (len : Size_t.t) : Size_t.t =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ Size_t.zero @@ fun k ->
  Luv.File.sendfile ~loop ~request self ~to_ ~offset len k

module Access_flag = struct
  type t =
    [ `F_OK
    | `R_OK
    | `W_OK
    | `X_OK
    ]

  let () = ignore (fun (x : t) : Luv.File.Access_flag.t -> x)
end

let access ?request (file : string) flags : unit =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ () @@ fun k -> Luv.File.access ~loop ~request file flags k

let chmod ?request (file : string) (modes : Mode.t list) : unit =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ () @@ fun k -> Luv.File.chmod ~loop ~request file modes k

let fchmod ?request (file : t) (modes : Mode.t list) : unit =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ () @@ fun k -> Luv.File.fchmod ~loop ~request file modes k

let utime ?request file ~atime ~mtime : unit =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ () @@ fun k -> Luv.File.utime ~loop ~request file ~atime ~mtime k

let futime ?request file ~atime ~mtime : unit =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ () @@ fun k -> Luv.File.futime ~loop ~request file ~atime ~mtime k

let lutime ?request file ~atime ~mtime : unit =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ () @@ fun k -> Luv.File.lutime ~loop ~request file ~atime ~mtime k

let link ?request file ~link : unit =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ () @@ fun k -> Luv.File.link ~loop ~request file ~link k

let symlink ?request file ~link : unit =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ () @@ fun k -> Luv.File.symlink ~loop ~request file ~link k

let readlink ?request file : string =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ "" @@ fun k -> Luv.File.readlink ~loop ~request file k

let realpath ?request file : string =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ "" @@ fun k -> Luv.File.realpath ~loop ~request file k

let chown ?request file ~uid ~gid : unit =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ () @@ fun k -> Luv.File.chown ~loop ~request file ~uid ~gid k

let lchown ?request file ~uid ~gid : unit =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ () @@ fun k -> Luv.File.lchown ~loop ~request file ~uid ~gid k

let fchown ?request file ~uid ~gid : unit =
  let loop = get_cur_luv_loop () in
  let@ request = with_request_opt ?request () in
  await_cb_ () @@ fun k -> Luv.File.fchown ~loop ~request file ~uid ~gid k

module Sync = Luv.File.Sync

let get_osfhandle : t -> (Luv__.Os_fd.Fd.t, Luv__.Error.t) result =
  Luv.File.get_osfhandle

let open_osfhandle : Luv__.Os_fd.Fd.t -> (t, Luv__.Error.t) result =
  Luv.File.open_osfhandle

let to_int : t -> int = Luv.File.to_int
let from_int : int -> t = Luv.File.from_int
