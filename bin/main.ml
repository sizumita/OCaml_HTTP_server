let test = "GET / HTTP/1.1
Host: localhost:8080
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Upgrade-Insecure-Requests: 1
Cookie: Webstorm-2ed3fb11=6b270cb0-2682-48ce-9400-cb50d0b2c9a5
User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.0 Safari/605.1.15
Accept-Language: ja
Accept-Encoding: gzip, deflate
Connection: keep-alive

abc"
let () = match Http_server.Http_lexer.parse_http_request test with
| exception Http_server.Http_lexer.ParseError e -> begin
  print_endline @@ Http_server.Http_lexer.string_of_ParseError e;
  ()
end
| _ -> ()
