{
  open Lexing
  open Parser_heap

  exception Error of string
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let int = ['0'-'9'] ['0'-'9']*

rule token = parse
  | "null" { NULL }
  | white { token lexbuf }
  | newline { new_line lexbuf ; NL }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | ['_' 'a'-'z' 'A'-'Z']['_' 'A'-'Z' 'a'-'z' '0'-'9' '\'']* as i { ID(i) }
  | _ { raise (Error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof  { EOF }
