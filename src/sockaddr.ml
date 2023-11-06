include Luv.Sockaddr

let ipv4_exn host port : t = ipv4 host port |> Err.unwrap_luv
let ipv6_exn host port : t = ipv6 host port |> Err.unwrap_luv
