open Graph
open Z3

open Ca

let mk_numeral ctx n  = Expr.mk_numeral_int ctx n (Arithmetic.Integer.mk_sort ctx)
let mk_const ctx num_prime id = 
  let prime num_primes id = id ^ (String.make num_primes '\'') in
  Arithmetic.Integer.mk_const_s ctx (prime num_prime id)

let identity_rel ctx num_prime vars =
  let consts  = List.map (mk_const ctx num_prime) vars in
  let consts' = List.map (mk_const ctx (num_prime+1)) vars in
  let constr  = List.map2 (Boolean.mk_eq ctx) consts' consts in
  constr

let rec torel_bexpr ctx num_prime = function
  | True  -> Boolean.mk_true ctx
  | False -> Boolean.mk_false ctx
  | Eq (id, n) -> Boolean.mk_eq ctx (mk_const ctx num_prime id) (mk_numeral ctx n)
  | Gt (id, n) -> Arithmetic.mk_gt ctx (mk_const ctx num_prime id) (mk_numeral ctx n)
  | Neg b -> Boolean.mk_not ctx (torel_bexpr ctx num_prime b)

let rec torel_nexpr ctx num_prime = function
  | Id id -> mk_const ctx num_prime id
  | Num n -> mk_numeral ctx n
  | Add (id, e) -> Arithmetic.mk_add ctx [ torel_nexpr ctx num_prime (Id id); torel_nexpr ctx num_prime e ]

let torel_stmt ctx vars num_prime stmt = match stmt with
  | Assume b -> Boolean.mk_and ctx ((torel_bexpr ctx num_prime b) :: (identity_rel ctx num_prime vars))
  | Asgn (id, e) ->
      let update = Boolean.mk_eq ctx (mk_const ctx (num_prime+1) id) (torel_nexpr ctx num_prime e) in
      let identity = identity_rel ctx num_prime (List.filter (fun v -> v <> id) vars) in
      Boolean.mk_and ctx (update :: identity)

let torel_seq ctx stmts vars =
  let body_const = Boolean.mk_and ctx (List.mapi (torel_stmt ctx vars) stmts) in
  let (--) i j = let rec aux n acc = if n < i then acc else aux (n-1) (n :: acc) in aux j [] in
  let primes_to_quantify = (1--((List.length stmts)-1)) in
  match primes_to_quantify with [] -> body_const | _ ->
    let qvar = List.fold_left (fun l num_primes -> l @ (List.map (mk_const ctx num_primes) vars)) [] primes_to_quantify in
    let quantifier = (Quantifier.mk_exists_const ctx qvar body_const None [] [] None None) in
    let expr = Quantifier.expr_of_quantifier quantifier in
    expr

module Concrete = struct

(* graph module declarations {{{ *)

type summary_id = int

module E_ = struct
  type t = (Expr.expr * int) * Cfg.edge_type
  let compare ((e1,_), s1) ((e2,_), s2) = let compe = Expr.compare e1 e2 in
    match compe with 0 -> compare s1 s2 | _ -> compe
  let default = (Boolean.mk_true (mk_context []), 0), Cfg.effect_id
end

module G = Persistent.Digraph.ConcreteBidirectionalLabeled(Ca.V_)(E_)

module Dot_ = Graphviz.Dot (struct
  include G
  let vertex_name (p,s) = string_of_int (Hashtbl.hash ((string_of_int p) ^ "__" ^ (pprint_structure s)))
  let graph_attributes _ = []
  let default_vertex_attributes _ = [`Shape `Box; `Regular false]
  let vertex_attributes c = [`Label (pprint_cloc ~sep:"\n" c)]
  let default_edge_attributes _ = []
  let edge_attributes (v1, ((e,_), summary), v2) = [ `Label (Expr.to_string e) ]
  let get_subgraph _ = None
end)
module Dot = struct
  include Dot_
  let write_dot g path = let chout = open_out path in Dot_.output_graph chout g ; close_out chout
end

(* }}} *)

let qe ctx num_stmt vars expr =
  let open Tactic in
  let open ApplyResult in
  let open Goal in
  let var' = List.map (mk_const ctx 1) vars in
  let var_primed = List.map (mk_const ctx num_stmt) vars in
  let goal = mk_goal ctx false false false in
  Goal.add goal [ expr ];
  let ar = apply (mk_tactic ctx "qe-light") goal None in
  assert ((get_num_subgoals ar) = 1) ;
  let expr = Goal.as_expr (get_subgoal ar 0) in
  let sub_expr = Expr.substitute expr var_primed var' in
  Expr.simplify sub_expr None

let of_ca ctx ?(do_qe=true) ca =
  let vars = collect_vars ca in
  Ca.G.fold_edges_e (fun (from, (stmts, summary), to_) ca_rel ->
    let highest_prime = List.length stmts in
    let expr = torel_seq ctx stmts vars in
    if do_qe then 
      let qe_expr = qe ctx highest_prime vars expr in
      G.add_edge_e ca_rel (from, ((qe_expr, 1), summary), to_)
    else
      G.add_edge_e ca_rel (from, ((expr, highest_prime), summary), to_)
  ) ca G.empty

end

module Abstract = struct

type diff_constr = Strict | NonStrict | DontKnow
let pprint_diff_constr = function
  | Strict -> "<"
  | NonStrict -> "<="
  | DontKnow -> "?"
let pprint_diff_constr_list l =
  let l = List.filter (fun (_, c) -> c <> DontKnow) l in
  String.concat "\n" (
  List.map (fun (id, diff_constr) ->
    Printf.sprintf "%s' %s %s" id (pprint_diff_constr diff_constr) id
  ) l
)

(* graph module declarations {{{ *)

type summary_id = int

module E_ = struct
  type t = (identifier * diff_constr) list * Cfg.edge_type
  let compare = compare
  let default = [], Cfg.effect_id
end

module G = Persistent.Digraph.ConcreteBidirectionalLabeled(Ca.V_)(E_)

module Dot_ = Graphviz.Dot (struct
  include G
  let vertex_name (p,s) = string_of_int (Hashtbl.hash ((string_of_int p) ^ "__" ^ (pprint_structure s)))
  let graph_attributes _ = []
  let default_vertex_attributes _ = [`Shape `Box; `Regular false]
  let vertex_attributes c = [`Label (pprint_cloc ~sep:"\n" c)]
  let default_edge_attributes _ = []
  let edge_attributes (v1, (e, summary), v2) = [ `Label (pprint_diff_constr_list e) ]
  let get_subgraph _ = None
end)
module Dot = struct
  include Dot_
  let write_dot g path = let chout = open_out path in Dot_.output_graph chout g ; close_out chout
end

module SCC = Components.Make(G)

(* }}} *)

let abstract ctx vars expr highest_prime = List.map (fun var ->
  let check_diff_constr pred =
    let c = mk_const ctx 0 var in
    let c' = mk_const ctx highest_prime var in
    let constr = pred ctx c' c in
    let impl = Boolean.mk_implies ctx expr constr in
    let sat_problem = Boolean.mk_not ctx impl in
    (* Printf.printf "%s\n" (Expr.to_string sat_problem); *)
    let s = Solver.mk_solver ctx None in
    Solver.add s [ sat_problem ] ;
    let result = Solver.check s [] in
    result = Solver.UNSATISFIABLE (*, constr 1*)
  in
  let kind =
    if check_diff_constr Arithmetic.mk_le then
      if check_diff_constr Arithmetic.mk_lt then Strict
      else NonStrict
    else DontKnow
  in
  var, kind
) vars

let of_ca ?(do_qe=false) ca =
  let ctx = mk_context [] in
  let vars = collect_vars ca in
  let ca_rel = Concrete.of_ca ctx ~do_qe ca in
  Concrete.G.fold_edges_e (fun (from, ((expr, highest_prime), summary), to_) ca_rel ->
    let diff_constr = abstract ctx vars expr highest_prime in
    G.add_edge_e ca_rel (from, (diff_constr, summary), to_)
  ) ca_rel G.empty

end
