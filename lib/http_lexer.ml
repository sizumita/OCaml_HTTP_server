open Http_parser

let vchar = [%sedlex.regexp? 0x00 .. 0x7f]

type lexbuf = {
  stream : Sedlexing.lexbuf ;
  mutable pos : Lexing.position ;
}
let create_lexbuf ?(file="") stream =
  let pos = {Lexing.
    pos_fname = file;
    pos_lnum = 1; (* Start lines at 1, not 0 *)
    pos_bol = 0;
    pos_cnum = 0;
  }
  in { pos ; stream }

let new_line ?(n=0) lexbuf =
  let _ = n in
  let open Lexing in
  let lcp = lexbuf.pos in
  lexbuf.pos <-
    {lcp with
        pos_lnum = lcp.pos_lnum + 1;
        pos_bol = lcp.pos_cnum;
    }

let string_of_ParseError (file, line, cnum, tok) =
  let file_to_string file =
    if file = "" then ""
    else " on file " ^ file
  in
  Printf.sprintf
    "Parse error%s line %i, column %i, token %s"
    (file_to_string file)
    line cnum tok

let update lexbuf =
  let new_pos = Sedlexing.lexeme_end lexbuf.stream in
  let p = lexbuf.pos in
  lexbuf.pos <- {p with Lexing.pos_cnum = new_pos }

let lexeme { stream; _ } = Sedlexing.Utf8.lexeme stream

exception ParseError of (string * int * int * string)

let raise_ParseError lexbuf =
  let {pos; _} = lexbuf in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  let tok = lexeme lexbuf in
  raise @@ ParseError (pos.pos_fname, line, col, tok)

let rec header lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
    | '\n' ->
      update lexbuf; new_line lexbuf; content lexbuf

    (** 空白文字 *)
    | white_space ->
      update lexbuf;
      header lexbuf
    | ':' -> update lexbuf ; field_value lexbuf
    | Plus vchar -> update lexbuf ; FIELD_NAME (lexeme lexbuf)
    | eof -> update lexbuf ; EOF
    | _ -> update lexbuf; raise_ParseError lexbuf
and field_value lexbuf = 
  let buf = lexbuf.stream in
  match%sedlex buf with
  | white_space -> update lexbuf; field_value lexbuf
  | '\n' -> update lexbuf ; header lexbuf

  | Star (Compl '\n') -> update lexbuf; FIELD_CONTENT (lexeme lexbuf)
  | _ -> update lexbuf; raise_ParseError lexbuf
and start_line lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | '\n' -> update lexbuf; new_line lexbuf; header lexbuf
  | white_space -> update lexbuf; start_line lexbuf
  | "GET" -> update lexbuf; GET
  | Rep ('0'..'9', 3) -> update lexbuf; STATUS_CODE (int_of_string (lexeme lexbuf))
  | "HTTP/", '0'..'9', '.', '0'..'9' -> update lexbuf; HTTP_VERSION (lexeme lexbuf)
  | Plus (Compl white_space) -> update lexbuf; REQUEST_LINE_STRING (lexeme lexbuf)
  | eof -> update lexbuf; EOF
  | _ -> update lexbuf; raise_ParseError lexbuf
and content lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | Star (Compl eof) -> update lexbuf ; CONTENT (lexeme lexbuf)
  | eof -> update lexbuf ; EOF
  | _ -> update lexbuf; raise_ParseError lexbuf

let parse f lexbuf =
  let lexer () =
    let ante_position = lexbuf.pos in
    let token = start_line lexbuf in
    let post_position = lexbuf.pos
    in (token, ante_position, post_position) in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised f
  in
  try
    parser lexer
  with
    | Sedlexing.MalFormed
    | Sedlexing.InvalidCodepoint _
      -> failwith "parse error"

let parse_http_request text =
  let lexbuf = create_lexbuf @@ Sedlexing.Utf8.from_string text in
  parse Http_parser.request lexbuf
