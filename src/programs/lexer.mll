{
  open Lexing
  open Parser

  exception Error of string
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule token = parse
  | "def" { DEF }
  | "begin" { BEGIN }
  | "end" { END }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fi" { FI }
  | "while" { WHILE }
  | "do" { DO }
  | "od" { OD }
  | ":=" { ASGN }
  | "null" { NULL }
  | "new" { NEW }
  | ".next" { NEXT }
  | "break" { BREAK }
  | "CAS" { CAS }
  | "true" { TRUE }
  | "false" { FALSE }
  | "assume" { ASSUME }
  | '=' { EQ }
  | ';' { SEMI }
  | ',' { COMMA }
  | '!' { NEG }
  | '<' { LANGLE }
  | '>' { RANGLE }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | white { token lexbuf }
  | newline { new_line lexbuf ; token lexbuf }
  | ['_' 'a'-'z' 'A'-'Z']['_' 'A'-'Z' 'a'-'z' '0'-'9' '\'']* as i { ID(i) }
  | _ { raise (Error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof  { EOF }
