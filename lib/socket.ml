open Lwt

let try_close chan =
  catch (fun () -> Lwt_io.close chan)
  (function _ -> return ())

let create_socket (state_ : Http_state.http_state) =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  bind sock @@ ADDR_INET(Unix.inet_addr_of_string (state_.host), int_of_string state_.port) >>= fun () ->
  listen sock 10;
  return sock

let handle_message ic oc state () =
  Lwt_io.read_line_opt ic >>= fun msg ->
  match msg with
  | Some msg ->
    Http.handle msg state
    >>= (fun ret ->  Lwt_io.write_line oc ret)
    >>= (fun () -> try_close oc )
    >>= (fun () -> return_unit)
  | None -> return_unit

let handle_connection conn state =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt.ignore_result (handle_message ic oc state ());
  return_unit

let create_server sock state =
  let rec serve () =
    Lwt_unix.accept sock >>= fun x -> handle_connection x state >>= serve
  in serve
