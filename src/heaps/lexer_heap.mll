{
  open Lexing
  open Parser_heap

  exception Error of string
}

let blank = [' ' '\t' '\n' '\r']
let int = ['0'-'9'] ['0'-'9']*

rule token = parse
  | ';' { SEMI }
  | blank { token lexbuf }
  | "null" { NULL }
  | ['_' 'a'-'z' 'A'-'Z']['_' 'A'-'Z' 'a'-'z' '0'-'9' '\'']* as i { ID(i) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | _ { raise (Error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof  { EOF }
