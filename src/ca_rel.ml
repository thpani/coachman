open Ca_seq
open Util.Z3

(* type declarations {{{ *)

type highest_prime = int
type transrel = Expr.expr * highest_prime

(* }}} *)

(* statements to transition relations {{{ *)

let identity_rel ctx num_prime vars =
  let consts  = List.map (mk_const' ctx num_prime) vars in
  let consts' = List.map (mk_const' ctx (num_prime+1)) vars in
  let constr  = List.map2 (Boolean.mk_eq ctx) consts' consts in
  constr

let rec torel_bexpr ctx num_prime = function
  | True  -> Boolean.mk_true ctx
  | False -> Boolean.mk_false ctx
  | Eq (id, n) -> Boolean.mk_eq ctx (mk_const' ctx num_prime id) (mk_numeral ctx n)
  | Gt (id, n) -> Arithmetic.mk_gt ctx (mk_const' ctx num_prime id) (mk_numeral ctx n)
  | Neg b -> Boolean.mk_not ctx (torel_bexpr ctx num_prime b)

let rec torel_nexpr ctx num_prime = function
  | Id id -> mk_const' ctx num_prime id
  | Num n -> mk_numeral ctx n
  | Add (id, e) -> Arithmetic.mk_add ctx [ torel_nexpr ctx num_prime (Id id); torel_nexpr ctx num_prime e ]

let torel_stmt ctx vars num_prime stmt = match stmt with
  | Assume b -> Boolean.mk_and ctx ((torel_bexpr ctx num_prime b) :: (identity_rel ctx num_prime vars))
  | Asgn (id, e) ->
      let update = Boolean.mk_eq ctx (mk_const' ctx (num_prime+1) id) (torel_nexpr ctx num_prime e) in
      let identity = identity_rel ctx num_prime (List.filter (fun v -> v <> id) vars) in
      Boolean.mk_and ctx (update :: identity)

let torel_seq ctx stmts vars =
  let body_const = Boolean.mk_and ctx (List.mapi (torel_stmt ctx vars) stmts) in
  let (--) i j = let rec aux n acc = if n < i then acc else aux (n-1) (n :: acc) in aux j [] in
  let primes_to_quantify = (1--((List.length stmts)-1)) in
  match primes_to_quantify with [] -> body_const | _ ->
    let qvar = List.fold_left (fun l num_primes -> l @ (List.map (mk_const' ctx num_primes) vars)) [] primes_to_quantify in
    let quantifier = (Quantifier.mk_exists_const ctx qvar body_const None [] [] None None) in
    let expr = Quantifier.expr_of_quantifier quantifier in
    expr

(* }}} *)

(* graph module declarations {{{ *)

module G = Scfg.G(struct
  include Ca_vertex.Vertex

  type edge_label = transrel

  let compare_edge_label (e1,_) (e2,_) = Expr.compare e1 e2
  let equal_edge_label   (e1,_) (e2,_) = Expr.equal e1 e2
  let default_edge_label               = Boolean.mk_true (mk_context []), 0
end)

(* }}} *)

(* conversion from sequential CA {{{ *)

(** [qe ctx highest_prime vars expr] performs quantifier elimination on an expression [expr] over variables [vars], where variables with [1 <= highest_prime] primes are existentially quantified. The returned expression ranges over unprimed and (singly) primed variables. *)
let qe ctx highest_prime vars expr =
  let open Tactic in
  let open ApplyResult in
  let open Goal in
  let var' = List.map (mk_const' ctx 1) vars in
  let var_primed = List.map (mk_const' ctx highest_prime) vars in
  let goal = mk_goal ctx false false false in
  Goal.add goal [ expr ];
  let ar = apply (mk_tactic ctx "qe-light") goal None in
  assert ((get_num_subgoals ar) = 1) ;
  let expr = Goal.as_expr (get_subgoal ar 0) in
  let sub_expr = Expr.substitute expr var_primed var' in
  Expr.simplify sub_expr None

(** [of_seq ctx ca] converts a CA with sequential edge labels [ca] into one with transition relations as edge labels. *)
let of_seq ctx ?(do_qe=true) ca =
  let vars = collect_vars ca in
  Ca_seq.G.fold_edges_e (fun (from, (stmts, summary), to_) ca_rel ->
    let highest_prime = List.length stmts in
    let expr = torel_seq ctx stmts vars in
    if do_qe then 
      let qe_expr = qe ctx highest_prime vars expr in
      G.add_edge_e ca_rel (from, ((qe_expr, 1), summary), to_)
    else
      G.add_edge_e ca_rel (from, ((expr, highest_prime), summary), to_)
  ) ca G.empty

(* }}} *)
