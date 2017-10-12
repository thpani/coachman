%{
  open Heap
%}

%token <string> ID
%token <int> INT
%token NULL
%token SEMI
%token EOF

%start heap
%type <Heap.heap> heap
%%

heap:
  | EOF { [] }
  | vertices_list EOF { $1 }
  ;
  
vertices_list:
  | vertex SEMI                { [$1] }
  | vertex SEMI vertices_list  { $1 :: $3 }
  ;

vertex:
  | INT NULL ID { ($1, 0, $3) }
  | INT INT ID { ($1, $2, $3) }
  ;
