%token <string> ID
%token <int> INT
%token NULL
%token NL
%token EOF

%start heap
%type <(int * string) list> heap
%%

heap:
  | vertices_list EOF { $1 }
  ;
  
vertices_list:
  | { [] }
  | vertex NL vertices_list  { $1 :: $3 }
  ;

vertex:
  | NULL ID { (0, $2) }
  | INT ID { ($1, $2) }
  ;
