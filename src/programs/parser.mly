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
%token CONTINUE
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
  | LANGLE seq_stmt RANGLE SEMI               { Atomic ($2) }
  | ASSUME LPAREN bexpr RPAREN SEMI           { Assume ($3) }
  | BREAK SEMI                                { Break }
  | CONTINUE SEMI                             { Continue }
  | IF bexpr THEN seq_stmt ELSE seq_stmt FI   { IfThenElse ($2, $4, $6) }
  | IF bexpr THEN seq_stmt FI                 { IfThenElse ($2, $4, []) }
  | WHILE bexpr DO seq_stmt OD                { While ($2, $4) }
  | ID ASGN NEW SEMI                          { Alloc (Id $1) }
  | ID ASGN pexpr_null SEMI                   { Asgn (Id $1, $3) }
  | ID NEXT ASGN pexpr_null SEMI              { Asgn (Next $1, $4) }
  | CAS LPAREN pexpr COMMA ID COMMA ID COMMA ID RPAREN SEMI { IfThenElse(CAS($3, $5, $7, $9), [], []) }
  ;

pexpr:
  | ID NEXT { Next $1 }
  | ID      { Id $1 }
  ;

pexpr_null:
  | pexpr   { $1 }
  | NULL    { Null }
  ;

bexpr:
  | TRUE  { True }
  | FALSE { False }
  | pexpr EQ pexpr_null    { Eq($1, $3) }
  | NEG LPAREN bexpr RPAREN   { Neg($3) }
  | CAS LPAREN pexpr COMMA ID COMMA ID COMMA ID RPAREN    { CAS ($3, $5, $7, $9) }
  ;
