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
  | CASnext of identifier * identifier * identifier

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
  | CAS of identifier * identifier * identifier

type program = (identifier * stmt list) list

let dummy_var = "__l2ca_dummy"

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
  | CASnext (id1, id2, id3) -> Printf.sprintf "CAS(%s.next, %s, %s)" id1 id2 id3

let ttolit = function true -> True | false -> False

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
  | CAS (id1, id2, id3) -> Printf.sprintf "CAS(%s, %s, %s)" id1 id2 id3

let rec unwrap_atomic = function
  | Atomic s :: tl -> (unwrap_atomic s) @ (unwrap_atomic tl)
  | s1       :: tl -> s1 :: (unwrap_atomic tl)
  | []             -> []

(* Each pointer assignment of the form u := new, u := w, or u := w.next is
 * immediately preceded by an assignment of the form u := null. A pointer
 * assignment of the form u := u.next is turned into v := u; u := null; u :=
 * v.next, possibly introducing a fresh variable v. Each pointer assignment of
 * the form u.next := w is immediately preceded by u.next := null.
 *)
let rec precompile_seq ?(intr_atomic=true) stmts = List.concat (List.map (precompile ~intr_atomic) stmts)
and precompile ?(intr_atomic=true) = function
  | IfThenElse (g, sif, selse) -> [ IfThenElse (g, precompile_seq sif, precompile_seq selse) ]
  | While (g, s)               -> [ While (g, precompile_seq s) ]
  | Atomic s                   -> [ Atomic (precompile_seq ~intr_atomic:false s) ]
  | Break                      -> [ Break ]
  | Assume g                   -> [ Assume g ]
  | CAS (id1, id2, id3) -> [ IfThenElse (CAS(id1, id2, id3), [], []) ]
  | s -> let t = match s with
    | Alloc id                            -> [ AsgnNull id ; Alloc id ]
    | Asgn (id1, Id id2)  when id1 <> id2 -> [ AsgnNull id1; Asgn (id1, Id id2) ]
    | AsgnNext (id1, id2) when id1 <> id2 -> [ AsgnNull id1; AsgnNext (id1, id2) ]
    | AsgnNext (id1, id2) when id1 =  id2 -> [ AsgnNull dummy_var; Asgn (dummy_var, Id id1); AsgnNull id1; AsgnNext (id1, dummy_var); AsgnNull dummy_var ]
    | NextAsgnId (id1, id2) when id1 <> id2 -> [ NextAsgnNull id1 ; NextAsgnId (id1, id2) ]
    | _ -> assert false
  in if intr_atomic then [ Atomic t ] else t
