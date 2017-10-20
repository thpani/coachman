open Graph

(* type declarations {{{ *)

type identifier = string

type pexpr =
  | Null
  | Id of identifier
  | Next of identifier

type bexpr =
  | True
  | False
  | Eq of pexpr * pexpr
  | Neg of bexpr

type stmt =
  | Assume of bexpr
  | Alloc of pexpr
  | Asgn of pexpr * pexpr

type ploc = int
type summary_id = int

(* }}} *)

let ttolit = function true -> True | false -> False

(* Ast <-> Cfg type conversion {{{ *)

let from_ast_pexpr = function
  | Ast.Null -> Null
  | Ast.Id id -> Id id
  | Ast.Next id -> Next id

let rec from_ast_bexpr = function
  | Ast.True -> True
  | Ast.False -> False
  | Ast.Eq (a, b) -> Eq (from_ast_pexpr a, from_ast_pexpr b)
  | Ast.Neg a -> Neg (from_ast_bexpr a)
  | Ast.CAS _ -> raise (Invalid_argument "CAS should have been rewritten as atomic assume/assign")

let rec from_ast_stmt = List.map (function
  | Ast.Assume b -> Assume (from_ast_bexpr b)
  | Ast.Alloc p -> Alloc (from_ast_pexpr p)
  | Ast.Asgn (a, b) -> Asgn (from_ast_pexpr a, from_ast_pexpr b)
  | Ast.Break | Ast.Atomic _ | Ast.IfThenElse _ | Ast.While _ -> raise (Invalid_argument "Should not appear as CFG stmt")
)

let to_ast_pexpr = function
  | Null -> Ast.Null
  | Id id -> Ast.Id id
  | Next id -> Ast.Next id

let rec to_ast_bexpr = function
  | True  -> Ast.True
  | False -> Ast.False
  | Eq (a, b) -> Ast.Eq (to_ast_pexpr a, to_ast_pexpr b)
  | Neg a ->  Ast.Neg (to_ast_bexpr a)

let rec to_ast_stmt = List.map (function
  | Assume b -> Ast.Assume (to_ast_bexpr b)
  | Alloc p -> Ast.Alloc (to_ast_pexpr p)
  | Asgn (a, b) -> Ast.Asgn (to_ast_pexpr a, to_ast_pexpr b)
)

(* }}} *)

(* printing functions {{{ *)

let pprint_pexpr = function
  | Null    -> "null"
  | Id id   -> id
  | Next id -> id ^ ".next"

let rec pprint_bexpr = function
  | True  -> "true"
  | False -> "false"
  | Eq (a, b) -> Printf.sprintf "(%s) = (%s)" (pprint_pexpr a) (pprint_pexpr b)
  | Neg g     -> Printf.sprintf "!(%s)" (pprint_bexpr g)

let rec pprint_stmt ?(sep=";\n") = function
  | Assume g -> Printf.sprintf "assume(%s)" (pprint_bexpr g)
  | Alloc p      -> Printf.sprintf "%s := new" (pprint_pexpr p)
  | Asgn (a, b)  -> Printf.sprintf "%s := %s" (pprint_pexpr a) (pprint_pexpr b)

and pprint_seq ?(sep=";\n") stmts =
  String.concat sep (List.map (pprint_stmt ~sep:sep) stmts)

(* }}} *)

(* module declarations {{{ *)

module V_ = struct
  type t = ploc
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module E_ = struct
  type t = stmt list * summary_id
  let compare = compare
  let default = [ Assume True ], 0
end

module G = Imperative.Digraph.ConcreteBidirectionalLabeled(V_)(E_)
module GChecker = Path.Check(G)

module Dot_ = Graphviz.Dot (struct
  include G
  let vertex_name v = "\"" ^ (string_of_int v) ^  "\""
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes (v1, (stmt, summary), v2) = [
    `Label (pprint_seq stmt) ;
    `Color (Colormap.get_color summary) ;
    `Fontcolor (Colormap.get_color summary)
  ]
  let get_subgraph _ = None
end)

module Dot = struct
  include Dot_
  let write_dot g path = let chout = open_out path in Dot_.output_graph chout g ; close_out chout
end


(* }}} *)

(* AST precompilation {{{ *)

let dummy_var = "__l2ca_dummy"

(* Each pointer assignment of the form u := new, u := w, or u := w.next is
 * immediately preceded by an assignment of the form u := null. A pointer
 * assignment of the form u := u.next is turned into v := u; u := null; u :=
 * v.next, possibly introducing a fresh variable v. Each pointer assignment of
 * the form u.next := w is immediately preceded by u.next := null.
 *)
let rec precompile_seq stmts = List.concat (List.map precompile_stmt stmts)
and precompile_stmt = function
  | Assume g                   -> [ Assume g ]
  | Alloc Null                 -> raise (Invalid_argument "null is not a valid lvalue")
  | Alloc (Id id)              -> [ Asgn (Id id, Null) ; Alloc (Id id) ]
  | Alloc (Next _)             -> raise (Invalid_argument "allocation of .next not implemented")
  | Asgn (Null, _)             -> raise (Invalid_argument "null is not a valid lvalue")
  | Asgn (Id id1, Null)        -> [ Asgn (Id id1, Null) ]
  | Asgn (Id id1, Id id2)      -> if id1 <> id2 then [ Asgn (Id id1, Null); Asgn (Id id1, Id id2) ] else []
  | Asgn (Id id1, Next id2)    -> if id1 <> id2 then [ Asgn (Id id1, Null); Asgn (Id id1, Next id2) ] else precompile_seq [ Asgn (Id dummy_var, Id id1); Asgn (Id id1, Next dummy_var); Asgn (Id dummy_var, Null) ]
  | Asgn (Next id1, Null)      -> raise (Invalid_argument "assignment of .next := null not implemented")
  | Asgn (Next id1, Id id2)    -> if id1 <> id2 then [ Asgn (Next id1, Null) ; Asgn (Next id1, Id id2) ] else raise (Invalid_argument "assignment of .next not implemented")
  | Asgn (Next _, Next _)      -> raise (Invalid_argument "assignment of .next := .next not implemented")

let precompile cfg =
  G.iter_edges_e (fun edge ->
    let from, (stmt, summary), to_ = edge in
    G.remove_edge_e cfg edge ;
    G.add_edge_e cfg (from, (precompile_seq stmt, summary), to_)
  ) cfg

(* }}} *)

(* convert ast to cfg {{{ *)

let from_ast ast = 
  let last = ref 0 in
  let break_target = ref 0 in
  let did_break = ref false in
  let g = G.create() in
  let max_vertex g = G.fold_vertex (fun v a -> max v a) g 0 in
  let next_vertex g = (max_vertex g) + 1 in
  let rec gen_cfg ast =
    List.iter (fun stmt ->
      match stmt with
      | Ast.IfThenElse (guard, sif, selse) ->
          (* rewrite CAS statements used as conditional *)
          let if_guard, else_guard =
            let open Ast in
            match guard with
            | Ast.CAS (a,b,c) ->
                let succ_cas = Atomic [ Assume (Eq(a, Id b)) ; Asgn(a, Id c) ] in
                let fail_cas = Atomic [ Assume(Neg(Eq(a, Id b))) ] in
                succ_cas, fail_cas
            | _ -> Assume guard, Assume (Neg guard)
            in
            let sif = if_guard :: sif in
            let selse = else_guard :: selse in
            let last_before_if = !last in
            let last_after_if = ref 0 in
              gen_cfg sif ;
              last_after_if := !last ;
              last := last_before_if ;
              gen_cfg selse ;
              let nv = if !did_break then next_vertex g else !last_after_if in
              G.add_edge_e g (!last, ([Assume True], 0), nv) ;
                did_break := false ;
                last := nv
      | Ast.While(guard, stmts) ->
          let before_branch = !last in
          let after_branch = next_vertex g in
          let stmts = Ast.Assume(guard) :: stmts in
          break_target := after_branch ;
          G.add_edge_e g (!last, ([Assume(Neg (from_ast_bexpr guard))], 0), after_branch) ;
          gen_cfg stmts ;
          G.add_edge_e g (!last, ([Assume True], 0), before_branch) ;
          last := max_vertex g
      | Ast.Break ->
          G.add_edge_e g (!last, ([Assume True], 0), !break_target) ;
          did_break := true
      | Ast.Atomic stmt ->
        let next_v = next_vertex g in
        G.add_edge_e g (!last, (from_ast_stmt stmt, 0), next_v) ; last := next_v
      | stmt ->
        let next_v = next_vertex g in
        G.add_edge_e g (!last, (from_ast_stmt [stmt], 0), next_v) ; last := next_v
    ) ast
  in
  gen_cfg ast ;
  (* Remove assume(false) edges *)
  let rec bval = function
    | True -> Some True
    | False -> Some False
    | Neg s -> (match bval s with Some True -> Some False | Some False -> Some True | _ -> None)
    | _ -> None
  in
  G.iter_edges_e (fun (from, (stmt, _), to_) -> match stmt with
    | [ Assume s ] -> (match bval s with
      | Some False -> G.remove_edge g from to_
      | Some True  -> if from > 0 then
        let preds = G.pred_e g from in
        G.remove_edge g from to_ ;
        List.iter (fun (pred, s, pred_to) ->
            G.remove_edge g pred pred_to ;
            G.add_edge_e g (pred, s, to_) ;
        ) preds
      | _ -> ()
    )
    | _ -> ()
  ) g ;
  (* Remove unreachable vertices *)
  G.iter_vertex (fun v ->
    if not (GChecker.check_path (GChecker.create g) 0 v) then G.remove_vertex g v
  ) g ;
  g

(* }}} *)

let add_summaries cfg summaries =
  List.iteri (fun i (summary_name, summary) ->
    let summary_seq = Ast.unwrap_atomic summary in
    let summary_nested_list = from_ast_stmt summary_seq in
    G.iter_vertex (fun v -> G.add_edge_e cfg (v, (summary_nested_list, i+1), v)) cfg
  ) summaries
