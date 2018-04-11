open Ca_vertex
open Util.Z3

open Z3

(* type definitions {{{ *)

type variable = string
type sc_operator = Strict | NonStrict | DontKnow
type sc_predicate = variable * sc_operator
type sc_transition = sc_operator VariableMap.t

(* }}} *)

(* pretty printing {{{ *)

let pprint_operator = function Strict -> "<" | NonStrict -> "<=" | DontKnow -> "?"
let pprint_predicate (v, op) = Printf.sprintf "%s' %s %s" v (pprint_operator op) v
let pprint_transition l =
  let l = VariableMap.filter (fun _ c -> c <> DontKnow) l in
  String.concat "\n" (
  VariableMap.fold (fun variable predicate l ->
    pprint_predicate (variable,predicate) :: l
  ) l []
)

(* }}} *)

(* graph module declarations {{{ *)

module G = Scfg.G(struct
  include Vertex

  type edge_label = sc_transition

  let compare_edge_label = VariableMap.compare Pervasives.compare
  let equal_edge_label   = VariableMap.equal Pervasives.(=)
  let default_edge_label = VariableMap.empty
end)

(* }}} *)

(* size-change abstraction {{{ *)

(** [abstract ctx transrel highest_prime var] returns a size-change operator [op] if [var' op var] is implied by [transrel]. [transrel] ranges over vars with highest prime [highest_prime]. *)
let abstract ctx transrel highest_prime var =
  let qf_lia = Config.get_qf_lia () in
  let s = Solver.mk_solver ctx qf_lia in
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

module Z3ExprHashtbl = Hashtbl.Make(
  struct
    type t = Z3.Expr.expr
    let equal = Z3.Expr.equal
    let hash  = Hashtbl.hash
  end
  )

(** [abstract_vars ctx transrel highest_prime vars h] returns a map from variables [vars] to size-change predicates implied by transition relations [transrel]. [transrel] ranges over vars with highest prime [highest_prime]. Hashtable [h] is used to cache abstraction results. *)
let abstract_vars ctx transrel highest_prime vars h =
  let open Util.DS in
  IdentifierSet.fold (fun var map ->
      let abs = match Z3ExprHashtbl.find_opt h transrel with
        | Some op -> op
        | None ->
          begin
            let op = abstract ctx transrel highest_prime var in
            Z3ExprHashtbl.add h transrel op ; op
          end
      in
    IdentifierMap.add var abs map
  ) vars IdentifierMap.empty

(** [of_concrete ctx vars ca_rel] abstracts a CA with relational edge labels [ca_rel] over variables [vars] into a size-change system. *)
let of_rel ctx vars ca_rel =
  let h = Z3ExprHashtbl.create 10 in
  Ca_rel.G.fold_edges_e (fun (from, ((transrel, highest_prime), summary), to_) ca_rel ->
    let diff_constr = abstract_vars ctx transrel highest_prime vars h in
    G.add_edge_e ca_rel (from, (diff_constr, summary), to_)
  ) ca_rel G.empty

(** [of_seq ctx vars ca_seq] abstracts a CA with statement edge labels [ca_seq] over variables [vars] into a size-change system. *)
let of_seq ctx vars ca_seq =
  Debugger.debug "ca_sca" "seq -> rel\n%!" ;
  let ca_rel = Ca_rel.of_seq ctx ca_seq in
  Debugger.debug "ca_sca" "rel -> sca\n%!" ;
  let ca_sca = of_rel ctx vars ca_rel in
  Debugger.debug "ca_sca" "done\n%!" ;
  ca_sca

(* }}} *)
