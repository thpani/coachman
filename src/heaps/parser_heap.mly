%token <string> ID
%token <int> INT
%token COMMA
%token SEMI
%token LPAR
%token RPAR
%token EOF

%start heaps
%type <(int * int option * string list * int option) list list> heaps
%%

heaps:
  | EOF  { [] }
  | heap heaps { $1 :: $2 }

heap:
  | vertex_null SEMI { [ $1 ] }
  | vertex_null COMMA vertex_list { $1 :: $3 }

vertex_list:
  | vertex SEMI { [ $1 ] }
  | vertex COMMA vertex_list { $1 :: $3 }

vertex_null:
  | INT { ($1, None, [], None) }
  | INT LPAR id_list RPAR { ($1, None, $3, None) }

vertex:
  | INT INT { ($1, Some $2, [], None) }
  | INT INT LPAR id_list RPAR { ($1, Some $2, $4, None) }
  | INT INT LPAR id_list RPAR INT { ($1, Some $2, $4, Some $6) }

id_list:
  | { [] }
  | ID { [ $1 ] }
  | ID COMMA id_list { $1 :: $3 }
