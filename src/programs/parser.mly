%{
  open Ast
  let parse_error s = print_endline s
%}

%token <string> ID
%token IF
%token THEN
%token ELSE
%token FI
%token BREAK
%token WHILE
%token DO
%token OD
%token ASGN
%token NULL
%token TRUE
%token FALSE
%token NEW
%token NEXT
%token EQ
%token DOT
%token SEMI
%token NEG
%token EOF
%token LANGLE
%token RANGLE
%token CAS
%token COMMA
%token LPAREN
%token RPAREN
%token ASSUME
%token DEF
%token BEGIN
%token END

%start program
%type <Ast.program> program
%%

program:
  | EOF { [] }
  | fct program { $1 :: $2 }

fct:
  | DEF ID BEGIN seq_stmt END { ($2, $4) }
  ;
  
seq_stmt:
  |                    { [] }
  | statement seq_stmt { $1 :: $2 }
  ;

statement:
  | IF guard THEN seq_stmt ELSE seq_stmt FI   { IfThenElse ($2, $4, $6) }
  | IF guard THEN seq_stmt FI                 { IfThenElse ($2, $4, []) }
  | WHILE guard DO seq_stmt OD                { While ($2, $4) }
  | asgn SEMI                                 { $1 }
  | LANGLE seq_stmt RANGLE SEMI               { Atomic ($2) }
  | ASSUME LPAREN guard RPAREN SEMI           { Assume ($3) }
  | BREAK SEMI                                { Break }
  ;

asgn:
  | ID ASGN NULL { AsgnNull ($1) }
  | ID ASGN NEW  { Alloc ($1) }
  | ID ASGN ID   { Asgn ($1, Id $3) }
  | ID ASGN ID DOT NEXT   { AsgnNext ($1, $3) }
  | ID DOT NEXT ASGN NULL { NextAsgnNull ($1) }
  | ID DOT NEXT ASGN ID   { NextAsgnId ($1, $5) }
  ;

guard:
  | ID EQ ID    { Eq($1, Id $3) }
  | ID EQ NULL  { EqNull($1) }
  | NEG LPAREN guard RPAREN   { Neg($3) }
  | TRUE  { True }
  | FALSE { False }
  | CAS LPAREN ID COMMA ID COMMA ID RPAREN    { CAS ($3, $5, $7) }
  ;
