(* let () = Http_server.run () *)
let state = Http_server.Server.init "config.yml"

let () = Http_server.Server.run state
