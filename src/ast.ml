type identifier = string

type expr =
  | Id of identifier
  | Num of int
  | Add of identifier * expr

type guard =
  | Eq of identifier * expr
  | Gt of identifier * expr
  | EqNull of identifier
  | Neg of guard
  | True
  | False
  | CAS of identifier * identifier * identifier

type stmt =
  | IfThenElse of guard * stmt list * stmt list
  | While of guard * stmt list
  | AsgnNull of identifier
  | Alloc of identifier
  | Asgn of identifier * expr
  | AsgnNext of identifier * identifier
  | NextAsgnNull of identifier
  | NextAsgnId of identifier * identifier
  | Assume of guard
  | Atomic of stmt list
  | Break

type program = (identifier * stmt list) list

let rec pprint_expr = function
  | Id id -> id
  | Num num -> string_of_int num
  | Add (id, Id id2) -> Printf.sprintf "%s + %s" id id2
  | Add (id, Num n) when n >= 0 -> Printf.sprintf "%s + %d" id n
  | Add (id, Num n) when n < 0 -> Printf.sprintf "%s - %d" id (-n)
  | Add (id, expr) -> Printf.sprintf "%s + (%s)" id (pprint_expr expr)

let rec pprint_guard = function
  | Eq (id, expr) -> id ^ " = " ^ (pprint_expr expr)
  | Gt (id, expr) -> id ^ " > " ^ (pprint_expr expr)
  | EqNull id -> id ^ " = null"
  | Neg g -> "!(" ^ (pprint_guard g) ^ ")"
  | True -> "true"
  | False -> "false"
  | CAS (id1, id2, id3) -> Printf.sprintf "CAS(%s, %s, %s)" id1 id2 id3

let rec pprint_seq ?(sep=";\n") ?(atomic_angles=true) stmts = String.concat sep (List.map (pprint ~sep ~atomic_angles) stmts)
and pprint ?(sep=";\n") ?(atomic_angles=true) stmt = match stmt with
  | IfThenElse (g, sif, selse) -> String.concat "\n" [ "IF " ^ (pprint_guard g) ; pprint_seq sif ; "ELSE" ; pprint_seq selse ; "FI" ]
  | While (g, s)     -> String.concat "\n" [ "WHILE " ^ (pprint_guard g) ^ " DO" ; pprint_seq s ; "OD" ]
  | AsgnNull id      -> id ^ " := null"
  | Alloc id         -> id ^ " := new"
  | Asgn (id, expr)  -> id ^ " := " ^ (pprint_expr expr)
  | AsgnNext (id1, id2)   -> id1 ^ " := " ^ id2 ^ ".next"
  | NextAsgnNull id       ->  id ^ ".next := null"
  | NextAsgnId (id1, id2) -> id1 ^ ".next := " ^ id2
  | Assume g -> Printf.sprintf "assume(%s)" (pprint_guard g)
  | Atomic s -> if atomic_angles then "<" ^ (pprint_seq ~sep ~atomic_angles s) ^ ">" else pprint_seq ~sep ~atomic_angles s
  | Break -> "break"

let rec precompile_seq ?(intr_atomic=true) stmts = List.concat (List.map (precompile ~intr_atomic) stmts)
and precompile ?(intr_atomic=true) = function
  | IfThenElse (g, sif, selse) -> [ IfThenElse (g, precompile_seq sif, precompile_seq selse) ]
  | While (g, s)     -> [ While (g, precompile_seq s) ]
  | Alloc id         -> let x = [ AsgnNull id ; Alloc id ] in if intr_atomic then [ Atomic x ] else x
  | Asgn (id1, id2)  -> let x = [ AsgnNull id1; Asgn (id1, id2) ] in if intr_atomic then [ Atomic x ] else x
  | AsgnNext (id1, id2) -> let x = [ AsgnNull id1; AsgnNext (id1, id2) ] in if intr_atomic then [ Atomic x ] else x
  | NextAsgnId (id1, id2) -> let x = [ NextAsgnNull id1 ; NextAsgnId (id1, id2) ] in if intr_atomic then [ Atomic x ] else x
  | Atomic s -> [ Atomic (precompile_seq ~intr_atomic:false s) ]
  | x -> [ x ]
