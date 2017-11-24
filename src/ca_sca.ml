open Util.Z3

open Z3

type sc_operator = Strict | NonStrict | DontKnow
type sc_predicate = Cavertex.variable * sc_operator
type sc_transition = sc_operator Cavertex.VariableMap.t

let pprint_operator = function Strict -> "<" | NonStrict -> "<=" | DontKnow -> "?"
let pprint_predicate (v, op) = Printf.sprintf "%s' %s %s" v (pprint_operator op) v
let pprint_transition l =
  let l = Cavertex.VariableMap.filter (fun _ c -> c <> DontKnow) l in
  String.concat "\n" (
  Cavertex.VariableMap.fold (fun variable predicate l ->
    pprint_predicate (variable,predicate) :: l
  ) l []
)

(* graph module declarations {{{ *)

module G = Scfg.G(struct
  include Cavertex.GVertex

  type edge_label = sc_transition

  let compare_edge_label = Cavertex.VariableMap.compare Pervasives.compare
  let equal_edge_label   = Cavertex.VariableMap.equal Pervasives.(=)
  let default_edge_label = Cavertex.VariableMap.empty
  let pprint_edge_label  = pprint_transition

  let color_edge _  = 0
end)

(* }}} *)

let abstract ctx transrel highest_prime var =
    let s = Solver.mk_solver ctx None in
    let c = mk_const ctx var in
    let c' = mk_const' ctx highest_prime var in
    let constr = Arithmetic.mk_lt ctx c' c in
    let impl = Boolean.mk_implies ctx transrel constr in
    let sat_problem = Boolean.mk_not ctx impl in
    Solver.add s [ sat_problem ] ;
    match Solver.check s [] with
    | Solver.UNSATISFIABLE -> Strict
    | _ ->
      let constr = Arithmetic.mk_le ctx c' c in
      let impl = Boolean.mk_implies ctx transrel constr in
      let sat_problem = Boolean.mk_not ctx impl in
      Solver.add s [ sat_problem ] ;
      match Solver.check s [] with
      | Solver.UNSATISFIABLE -> NonStrict
      | _ -> DontKnow

let abstract_vars ctx transrel highest_prime vars =
  let open Cavertex in
  VariableSet.fold (fun var map ->
    VariableMap.add var (abstract ctx transrel highest_prime var) map
  ) vars VariableMap.empty

let of_concrete ctx vars ca_rel =
  let ca_rel_abstract = Ca_rel.G.fold_edges_e (fun (from, ((transrel, highest_prime), summary), to_) ca_rel ->
    let diff_constr = abstract_vars ctx transrel highest_prime vars in
    G.add_edge_e ca_rel (from, (diff_constr, summary), to_)
  ) ca_rel G.empty in
  ca_rel_abstract
