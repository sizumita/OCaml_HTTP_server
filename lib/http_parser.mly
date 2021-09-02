%{
open Http_ast
%}

%token <string> REQUEST_LINE_STRING HTTP_VERSION
%token <string> FIELD_NAME FIELD_CONTENT CONTENT
%token <int> STATUS_CODE
%token GET
%token EOF HEADER_START

%start <Http_ast.http_request> request

%%

request:
  | line = start_line; headers = list(header); content = option(CONTENT) EOF { Request (line, headers, content) }
  | error
    { failwith 
          (Printf.sprintf "parse error at line %d column %d"
              ($startpos.pos_lnum) ($startpos.pos_cnum - $startpos.pos_bol)
              )}

start_line:
  | GET target = REQUEST_LINE_STRING; version = HTTP_VERSION { Get (target, version) }

header:
  | name = FIELD_NAME; content = list(FIELD_CONTENT) { Header (name, content) }

