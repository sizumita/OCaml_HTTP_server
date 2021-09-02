type expr =
  | Null

type start_line =
  | Get of string * string

type header =
  | Header of string * string list

type http_request = 
  | Request of start_line * header list * string option
