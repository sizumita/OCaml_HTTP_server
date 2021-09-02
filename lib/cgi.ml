open Lwt
open Lwt.Syntax

let read_file path =
  Lwt.catch
    (fun _ ->
      let* channel = Lwt_io.open_file ~mode:Lwt_io.Input path in
      let* content = Lwt_io.read channel in
      let* () = Lwt_io.close channel in
      Lwt.return_ok content)
    (fun _ -> Lwt.return_error ())

let run_python_cgi file env =
  let process = Lwt_process.open_process_in ~env:env (Lwt_process.shell file) in
  Lwt_io.read  process#stdout
  >>= ( fun str -> Lwt_io.flush Lwt_io.stdout >>= (fun () -> Lwt.return str))

let get_env request env_name name =
  let Http_ast.Request(_, headers, _) = request in
  match headers |> List.filter (fun (Http_ast.Header(name_, _)) -> name_ = name) with
  | (Http_ast.Header (_, m)) :: _ -> m |> String.concat " " |> fun x -> env_name ^ "=" ^ x
  | _ -> env_name ^ "=" ^ ""

let make_cgi_env (req: Http_ast.http_request) =
  [|
    get_env req "HTTP_ACCEPT" "Accept";
  |]

let run request file =
  let env = make_cgi_env request in
  match String.split_on_char '.' file |> List.rev |> List.hd with
    | "py" -> run_python_cgi file env >>= (fun str -> Lwt.return str)
    | "html" -> read_file file
    >>= (fun r -> match r with
    | Ok c -> Lwt.return c
    | _ -> Lwt.return "")
    | _ -> Lwt.return "Unsupported file"
