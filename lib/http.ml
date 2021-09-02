open Http_ast
open Lwt

let parse_url url =
  if url = "/" then "/index.html" else
  let paths = String.split_on_char '/' url in
  paths |> List.filter (fun (path) -> path != "..") |> String.concat "/"

let handle_get request (state : Http_state.http_state) =
  let Request (Get(url, _), _, _) = request in
  let low_url = (String.lowercase_ascii url) |> parse_url in
  print_endline (state.cgi_path ^ low_url);
  match Sys.file_exists (state.cgi_path ^ low_url) with
  | false -> Lwt.return "HTTP/1.1 404 Not Found\nContent-Type: text/html; charset=UTF-8\n\n<p>Not Found</p>"
  | true -> (Cgi.run request (state.cgi_path ^ low_url)) >>= (fun str -> Lwt.return ("HTTP/1.1 200 OK\nContent-Type: text/html\n\n" ^ str))

let handle text state =
  let request = Http_lexer.parse_http_request text in
    (* ここでバリデーションをするべき *)
  match request with
  | Request(Get _, _, _) -> handle_get request state >>= ( fun str -> return str )
