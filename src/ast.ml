(* {{{ type declarations *)

type identifier = string

type pexpr =
  | Null
  | Id of identifier
  | Next of identifier

type bexpr =
  | True
  | False
  | Nondet
  | Eq of pexpr * pexpr
  | Neg of bexpr
  | And of bexpr * bexpr
  | CAS of pexpr * identifier * identifier * identifier

type stmt =
  | Atomic of stmt list
  | Assume of bexpr
  | Break
  | Continue
  | Return
  | IfThenElse of bexpr * stmt list * stmt list
  | While of bexpr * stmt list
  | Alloc of pexpr
  | Asgn of pexpr * pexpr

type seq = stmt list

type program = (identifier * seq) list

(* }}} *)

(* printing functions {{{ *)

let pprint_pexpr = function
  | Null    -> "null"
  | Id id   -> id
  | Next id -> id ^ ".next"

let rec pprint_bexpr = function
  | True  -> "true"
  | False -> "false"
  | Nondet -> "*"
  | Eq (a, b) -> Printf.sprintf "(%s) = (%s)" (pprint_pexpr a) (pprint_pexpr b)
  | Neg g     -> Printf.sprintf "!(%s)" (pprint_bexpr g)
  | And (a, b) -> Printf.sprintf "(%s && %s)" (pprint_bexpr a) (pprint_bexpr b)
  | CAS (a, b, c, _) -> Printf.sprintf "CAS(%s, %s, %s)" (pprint_pexpr a) b c

let rec pprint_stmt ?(sep=";\n") = function
  | Atomic s -> Printf.sprintf "< %s >" (pprint_seq ~sep:"; " s)
  | Assume g -> Printf.sprintf "assume(%s)" (pprint_bexpr g)
  | Break    -> "break"
  | Continue -> "continue"
  | Return -> "return"
  | IfThenElse (CAS(a, b, c, _), [], []) -> Printf.sprintf "CAS(%s, %s, %s)" (pprint_pexpr a) b c
  | IfThenElse (g, sif, selse) -> String.concat "\n" [ "if " ^ (pprint_bexpr g) ; pprint_seq ~sep sif ; "else" ; pprint_seq ~sep selse ; "fi" ]
  | While (g, s)     -> String.concat "\n" [ "while " ^ (pprint_bexpr g) ^ " do" ; pprint_seq ~sep s ; "od" ]
  | Alloc p      -> Printf.sprintf "%s := new" (pprint_pexpr p)
  | Asgn (a, b)  -> Printf.sprintf "%s := %s" (pprint_pexpr a) (pprint_pexpr b)

and pprint_seq ?(sep=";\n") stmts =
  String.concat sep (List.map (pprint_stmt ~sep:sep) stmts)

let pprint_function (fname, stmts) = Printf.sprintf "DEF %s BEGIN\n%s\nEND" fname (pprint_seq stmts)

let pprint_program p = String.concat "\n" (List.map pprint_function p)

(* }}} *)

(** Transform all (nested) atomic statements to normal statements. *)
let rec unwrap_atomic = function
  | Atomic s :: tl -> (unwrap_atomic s) @ (unwrap_atomic tl)
  | s1       :: tl -> s1 :: (unwrap_atomic tl)
  | []             -> []

(** Build non-deterministic switch between functions. *)
let rec build_nondet_switch = function
  | [] -> []
  | [ (_,ast) ]   -> ast
  | (_,ast1) :: tl -> [ IfThenElse (Nondet, ast1, build_nondet_switch tl) ]
