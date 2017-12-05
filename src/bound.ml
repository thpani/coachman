open Ca_sca
open Ca_vertex
open Debugger
open Util

module VariableSet = Util.DS.IdentifierSet
module VariableMap = Util.DS.IdentifierMap
module EnvBoundMap = Util.DS.IdentifierMap

type var_abs_map = Apron.Interval.t VariableMap.t
type ca_loc_with_constraints = Ca_vertex.ca_loc * var_abs_map

module Candidate = struct
  type bound = Const of int | Var of VariableSet.t * int option VariableMap.t | Unbounded
end

type bound = Bound of Z3.Expr.expr | Unbounded

let const_bound ctx i = Bound (Util.Z3.mk_numeral ctx i)
let const_bound_0 ctx = const_bound ctx 0
let const_bound_1 ctx = const_bound ctx 1

(* Simplify max expressions so we don't have lots of its in the bound expression *)
let max_expr ctx a b =
  let open Z3 in
  let ngt0 = Arithmetic.mk_gt ctx (Z3.mk_const ctx "N") (Z3.mk_numeral ctx 1) in
  let ageb = Arithmetic.mk_ge ctx a b in
  let bgea = Arithmetic.mk_ge ctx b a in
  if Z3.check_valid ctx (Boolean.mk_implies ctx ngt0 ageb) then
    a
  else if Z3.check_valid ctx (Boolean.mk_implies ctx ngt0 bgea) then
    b
  else
    Boolean.mk_ite ctx (Arithmetic.mk_gt ctx a b) a b

let min_expr ctx a b =
  let open Z3 in
  let ngt0 = Arithmetic.mk_gt ctx (Z3.mk_const ctx "N") (Z3.mk_numeral ctx 1) in
  let ageb = Arithmetic.mk_ge ctx a b in
  let bgea = Arithmetic.mk_ge ctx b a in
  if Z3.check_valid ctx (Boolean.mk_implies ctx ngt0 ageb) then
    b
  else if Z3.check_valid ctx (Boolean.mk_implies ctx ngt0 bgea) then
    a
  else
    Boolean.mk_ite ctx (Arithmetic.mk_gt ctx a b) b a

let max_bound ctx =
  List.fold_left (fun max_bound bound ->
    match (max_bound, bound) with
    | Unbounded, _         -> Unbounded
    |         _, Unbounded -> Unbounded
    | Bound e1, Bound e2 -> Bound (max_expr ctx e1 e2)
  ) (const_bound_0 ctx)

let min_bound ctx = function
  | [] -> const_bound_0 ctx
  | l -> List.fold_left (fun min_bound bound ->
    match min_bound, bound with
    | Unbounded, _ -> bound
    | _, Unbounded -> min_bound
    | Bound e1, Bound e2 -> Bound (min_expr ctx e1 e2)
  ) Unbounded l

module Complexity = struct
  type t = Const of int | Linear of string | Polynomial of (string * int) list | Unbounded | Unknown
  let rank = function
    | Const _      -> 10
    | Linear _     -> 20
    | Polynomial _ -> 30
    | Unbounded    -> 99
    | Unknown      -> failwith "Unknown is greater AND smaller than all others, thus has no rank"

  let pprint = function
    | Const i      -> Printf.sprintf "O(%d)" (min i 1)
    | Linear s     -> Printf.sprintf "O(%s)" s
    | Polynomial l -> (match l with [ (id, pow) ] -> Printf.sprintf "%s^%d" id pow | _ -> "???")
    | Unbounded    -> "∞"
    | Unknown      -> "???"

  let compare a b op = match (a,b) with
  | (Unknown,_) -> Unknown
  | (_,Unknown) -> Unknown
  | _ ->
      if op (rank a) (rank b) then a
      else if op (rank b) (rank a) then b
      else failwith "not implemented"

  let max a b = compare a b (>)
  let min a b = compare a b (<)
end

let symbol_to_string e =
  let open Z3 in
  Symbol.get_string (FuncDecl.get_name (Expr.get_func_decl e))

let get_const e =
  if Z3.Expr.is_numeral e then
    Some (Complexity.Const (Z3.Arithmetic.Integer.get_int e))
  else if Z3.Expr.is_const e then
    Some (Complexity.Linear (symbol_to_string e))
  else
    None

let rec get_complexity_of_expr e =
  let e = Z3.Expr.simplify e None in
  (* e is an addition of a numeral, constant, or multiplication of a numeral and constants *)
  match get_const e with
  | Some c -> c
  | None ->
    if Z3.Arithmetic.is_add e then
      let args = Z3.Expr.get_args e in
      let last_arg = List.nth args ((List.length args) - 1) in
      (* e is in sum-of-monomials format, so this is the highest degree term *)
      match get_const last_arg with
      | Some c -> c
      | None ->
        (* assume last_arg is a multiplication strip out possible constant in multiplication *)
        if Z3.Arithmetic.is_mul last_arg then
          let mul_args = Z3.Expr.get_args last_arg in
          let constants = match mul_args with
            | first_arg :: tail_arg ->
                if Z3.Expr.is_numeral first_arg then
                  List.fold_left (fun l arg -> (Z3.Expr.get_args arg) @ l) [] tail_arg
                else
                  mul_args
            | [] -> assert false
          in
          let occ_map = List.fold_left (fun map symbol ->
            let id = symbol_to_string symbol in
            let occ = match VariableMap.find_opt id map with Some i -> i+1 | None -> 1 in
            VariableMap.add id occ map
          ) VariableMap.empty constants
          in
          Complexity.Polynomial (VariableMap.bindings occ_map)
        else 
          Complexity.Unknown
    else if Z3.Boolean.is_ite e then
      let ite_args = Z3.Expr.get_args e in
      let comparison, term1, term2 = List.nth ite_args 0, List.nth ite_args 1, List.nth ite_args 2 in
      if Z3.Arithmetic.is_le comparison then
        let comparison_args = Z3.Expr.get_args comparison in
        let comparison_arg1, comparison_arg2 = List.nth comparison_args 0, List.nth comparison_args 1 in
        let minmax_op = 
          if Z3.Expr.is_numeral comparison_arg1 && Z3.Expr.is_const comparison_arg2 then
            Some Complexity.max
          else if Z3.Expr.is_const comparison_arg1 && Z3.Expr.is_numeral comparison_arg2 then
            Some Complexity.min
          else
            None
        in
        match minmax_op with
        | None -> Complexity.Unknown
        | Some minmax_op ->
            minmax_op (get_complexity_of_expr term1) (get_complexity_of_expr term2)
      else
        Complexity.Unknown
    else Complexity.Unknown

let bound_complexity = function
  | Unbounded -> Complexity.Unbounded
  | Bound e ->  get_complexity_of_expr e

let pprint_bound_asymp b = Complexity.pprint (bound_complexity b)

let pprint_bound = function
  | Bound e -> Z3.Expr.to_string (Z3.Expr.simplify e None)
  | Unbounded -> "∞"

let itvl_width itvl =
  let lower, upper = itvl.Apron.Interval.inf, itvl.Apron.Interval.sup in
  if (Apron.Scalar.is_infty lower) <> 0 || (Apron.Scalar.is_infty upper) <> 0 then
    None
  else
    let lower = match lower with Apron.Scalar.Mpqf i -> i | _ -> assert false in
    let upper = match upper with Apron.Scalar.Mpqf i -> i | _ -> assert false in
    let lower = int_of_string (Mpqf.to_string lower) in
    let upper = int_of_string (Mpqf.to_string upper) in
    Some (upper - lower)

let get_ub_invariant var scc abs_map =
  Ca_seq.G.fold_vertex (fun vertex greatest_width ->
    match greatest_width with
    | None -> None
    | Some greatest_width ->
      let apron_var = Apron.Var.of_string var in
      let absv = Ai.VertexMap.find vertex abs_map in
      let man = Apron.Abstract1.manager absv in
      let itvl = Apron.Abstract1.bound_variable man absv apron_var in
      match itvl_width itvl with
      | None -> None
      | Some this_width -> Some (max greatest_width this_width)
  ) scc (Some 0)

let get_ub_invariant_vars ranking_vars scc abs_map =
  VariableSet.fold (fun v ub_map ->
    VariableMap.add v (get_ub_invariant v scc abs_map) ub_map
  ) ranking_vars VariableMap.empty

let get_local_bounds ctx man env vars ca_seq abs_map =
  let open Ca_vertex in
  let sccs = Ca_seq.G.sccs ca_seq in
  let local_bounds = List.map (fun scc_seq ->
    let scc_seq = Ca_seq.propagate_equalities scc_seq in
    let scc_sca = Ca_sca.of_seq ctx vars scc_seq in
    let var_edge_map = VariableSet.fold (fun var map ->
      let edges_ranked_by_var = Ca_sca.G.filter_edge_e (fun edge -> (* edges on which variable `var' decreases without increasing anywhere in the SCC *)
        let _,(dc,_),_ = edge in
        let decreases_on_edge = (VariableMap.find var dc) = Ca_sca.Strict in
        let increases_in_scc = Ca_sca.G.fold_edges_e (fun (_,(dc,_),_) b -> b || ((VariableMap.find var dc) = Ca_sca.DontKnow)) scc_sca false in
        decreases_on_edge && (not increases_in_scc)
      ) scc_sca in
      VariableMap.add var edges_ranked_by_var map
    ) vars VariableMap.empty in
    (* VariableMap.iter (fun var edges_ranked_by_var -> *)
    (*   Printf.printf "Variable %s ranks edges:\n" var ; *)
    (*   List.iter (fun edge -> Printf.printf "  %s" (pprint_edge edge)) edges_ranked_by_var *)
    (* ) var_edge_map ; *)
    Ca_sca.G.fold_edges_e (fun edge map ->
      Debugger.debug "local_bound" "  Edge %s " (Ca_sca.G.pprint_edge edge) ;
      (* (2) Let v ∈ V. We define ξ(v) ⊆ E to be the set of all transitions τ = l1 → l2 ∈ E such that v' ≤ v + c ∈ u for some c < 0. For all τ ∈ ξ(v) we set ζ(τ) = v. *)
      let local_bound =
        let ranking_vars = VariableSet.filter (fun var ->
          let edges_ranked_by_var = VariableMap.find var var_edge_map in
          Ca_sca.G.mem_edge_e edges_ranked_by_var edge
        ) vars in
        if VariableSet.is_empty ranking_vars then begin
          Debugger.debug "local_bound" "ranked by none. " ;
          (* (3) Let v ∈ V and τ ∈ E. Assume τ was not yet assigned a local bound by (1) or (2). We set ζ(τ) = v, if τ does not belong to a strongly connected component (SCC) of the directed graph (L, E′) where E′ = E \ {ξ(v)} (the control flow graph of ∆P without the transitions in ξ(v)). *)
          let ranking_vars = VariableSet.filter (fun var ->
            let edges_ranked_by_var = VariableMap.find var var_edge_map in
            let scc_without_edges_ranked_by_var = Ca_sca.G.filter_edge_e (fun edge ->
              let is_edge_ranked_by_var = Ca_sca.G.mem_edge_e edges_ranked_by_var edge in
              not is_edge_ranked_by_var
            ) scc_sca in
            let var_breaks_scc = not (Ca_sca.G.edge_in_scc edge scc_without_edges_ranked_by_var) in
            var_breaks_scc
          ) vars in
          if VariableSet.is_empty ranking_vars then begin
            Debugger.debug "local_bound" "Unbounded.\n" ;
            Candidate.Unbounded
          end else begin
            let inv_ranking_vars = get_ub_invariant_vars ranking_vars scc_seq abs_map in
            Debugger.debug "local_bound" "Vars breaking SCC: %s\n" (Util.DS.IdentifierSet.pprint ranking_vars) ;
            Candidate.Var (ranking_vars, inv_ranking_vars)
          end
        end else begin
          let inv_ranking_vars = get_ub_invariant_vars ranking_vars scc_seq abs_map in
          Debugger.debug "local_bound" "ranked by %s.\n" (Util.DS.IdentifierSet.pprint ranking_vars) ;
          Candidate.Var (ranking_vars, inv_ranking_vars)
        end
      in
      let (f,_),(_,ek),(t,_) = edge in
      ((f,ek,t), local_bound) :: map
    ) scc_sca []
  ) sccs in
  let local_bounds = List.concat local_bounds in
  (* edges not appearing in SCC have constant bound 1 *)
  Ca_seq.G.fold_edges_e (fun ((f,_),(_,ek),(t,_)) l ->
    let local_bound = match List.assoc_opt (f,ek,t) local_bounds with
    | Some lb -> lb
    | None -> Candidate.Const 1
    in
    ((f,ek,t),local_bound) :: l
  ) ca_seq []

let summary_ctr summary_name = Printf.sprintf "summary_ctr_%s" summary_name
let is_summary_ctr id = Str.string_match (Str.regexp "^summary_ctr_\\(.*\\)") id 0
let get_summary_of_summary_ctr id =
  if is_summary_ctr id then Some (Str.matched_group 1 id)
  else None 

let pprint_env_bound_map ctx map = 
  String.concat "; " (List.map (fun (summary_name, bound) ->
  let bound = match bound with
  | Unbounded -> Unbounded
  | Bound expr ->
    let params = Z3.Params.mk_params ctx in
    Z3.Params.add_bool params (Z3.Symbol.mk_string ctx "som") true ;
    Bound (Z3.Expr.simplify expr (Some params))
  in
  Printf.sprintf "%s = %s" (summary_ctr summary_name) (pprint_bound bound)
) (EnvBoundMap.bindings map))

let minimal_hitting_set_approx var_sets =
  (* Select minimal set of variables present in all Var bounds.
   * The optimal solution would be computing the hitting set (NP-complete, Karp'72)
   * We approximate this by checking the intersection over all variable sets. *)
  match var_sets with
  | var_set :: _ ->  (
      let intersection = List.fold_left VariableSet.inter var_set var_sets in
      assert (not (VariableSet.is_empty intersection)) ;
      intersection
  )
  | [] -> (* no edges ranked by vars, hitting set is empty*)
      VariableSet.empty

let max_map_element list_of_maps key =
  List.fold_left (fun max_width map ->
    let width = VariableMap.find key map in
    match (max_width, width) with
    | None, _ -> None
    | _, None -> None
    | Some i, Some j -> Some (max i j)
  ) (Some 0) list_of_maps

let variable_bound ctx env_bound_map invs self_loop var =
  let bound = match get_summary_of_summary_ctr var with
  | Some summary ->
      (* var is an environment counter, by construction we can look its
        * upper bound invariant up in the env bound map. *)
      EnvBoundMap.find summary env_bound_map
  | None ->
      (* var is a CA counter, get its upper bound invariant from the
        * interval abstract interpretation pass. *)
      match max_map_element invs var with
      | Some c -> Bound (Z3.mk_numeral ctx c)
      | None -> Unbounded
  in
  (* if the transition is not a self-loop, add +1 for edges *before* the test *)
  match (bound, self_loop) with
  | Bound e, true -> Bound e
  | Bound e, false -> Bound (Z3.Arithmetic.mk_add ctx [ e ; (Z3.mk_numeral ctx 1)])
  | Unbounded, _ -> Unbounded

let fold_bounds ctx env_bound_map local_bounds_per_scc self_loop =
  let const, unbounded, var_sets, invs =
    List.fold_left (fun (const, unbounded, var_sets, invs) bound ->
      let open Candidate in match bound with
      | Const i            -> const + i, unbounded, var_sets           , invs
      | Unbounded          -> const    , true     , var_sets           , invs
      | Var (var_set, inv) -> const    , unbounded, var_set :: var_sets, inv :: invs
    ) (0,false,[],[]) local_bounds_per_scc in
  if unbounded then Unbounded
  else
    let vars = VariableSet.elements (minimal_hitting_set_approx var_sets) in
    let var_bounds = List.map (variable_bound ctx env_bound_map invs self_loop) vars in
    match min_bound ctx var_bounds with
    | Unbounded -> Unbounded
    | Bound min_var_bound_expr ->
      let c_expr = Util.Z3.mk_numeral ctx const in
      let sum_expr = Z3.Arithmetic.mk_add ctx [c_expr ; min_var_bound_expr] in
      let params = Z3.Params.mk_params ctx in
      Z3.Params.add_bool params (Z3.Symbol.mk_string ctx "som") true ;
      Bound (Z3.Expr.simplify sum_expr (Some params))

let multiply_by_env_num ctx = function
  | Unbounded  -> Unbounded
  | Bound expr ->
      let open Util in
      let n = Z3.mk_const ctx "N" in
      let one = Z3.mk_numeral ctx 1 in
      let env_num = Z3.Arithmetic.mk_sub ctx [ n ; one ] in
      let mult_expr = Z3.Arithmetic.mk_mul ctx [ expr ; env_num ] in
      let params = Z3.Params.mk_params ctx in
      Z3.Params.add_bool params (Z3.Symbol.mk_string ctx "som") true ;
      Bound (Z3.Expr.simplify mult_expr (Some params))

(* let refine_ca_rel_with_env_bounds ctx ca_rel env_bound_map = *)
(*   let env_counter_constraint edge_type highest_prime = *)
(*     EnvBoundMap.fold (fun summary_name bound result -> *)
(*       let summary_ctr = summary_ctr summary_name in *)
(*       let summary_ctr_expr = Ca_rel.mk_const ctx 0 summary_ctr in *)
(*       let summary_ctr_expr' = Ca_rel.mk_const ctx highest_prime summary_ctr in *)
(*       let bound = EnvBoundMap.find summary_name env_bound_map in *)
(*       let rhs = if bound <> Unbounded && edge_type = Cfg.S summary_name then *)
(*         Z3.Arithmetic.mk_add ctx [ summary_ctr_expr ; Ca_rel.mk_numeral ctx (-1) ] *)
(*       else *)
(*         summary_ctr_expr *)
(*       in *)
(*       (Z3.Boolean.mk_eq ctx summary_ctr_expr' rhs) :: result *)
(*     ) env_bound_map [] *)
(*   in *)
(*   let open Ca_rel.Concrete in *)
(*   G.fold_edges_e (fun (from, ((expr, highest_prime), edge_type), to_) ca_rel -> *)
(*     let expr' = Z3.Boolean.mk_and ctx (expr :: env_counter_constraint edge_type highest_prime) in *)
(*     let edge' = (from, ((expr', highest_prime), edge_type), to_) in *)
(*     G.add_edge_e ca_rel edge' *)
(*   ) ca_rel G.empty *)

(** For bounded CA summary edges, refine constraints on initial states, i.e., on summary counters *)
let refine_init_constraints_with_env_bounds constraints env_bound_map =
  EnvBoundMap.fold (fun summary_name bound acc_constr ->
    match bound with
    | Bound expr ->
        if Z3.Expr.is_numeral expr then
          let bound = Z3.Arithmetic.Integer.get_int expr in
          let interval = Apron.Interval.of_int 0 bound in
          let var_name = summary_ctr summary_name in
          VariableMap.add var_name interval acc_constr
        else
          acc_constr
    | Unbounded -> acc_constr
  ) env_bound_map constraints

(** For bounded CA summary edges, add decrement statement of the summary_counter *)
let refine_ca_with_env_bounds ca env_bound_map =
  let env_counter_stmt edge_type = 
    match edge_type with
    | Scfg.S summary_name -> 
        let bound = EnvBoundMap.find summary_name env_bound_map in
        let summary_ctr = summary_ctr summary_name in
        if bound <> Unbounded then 
          let open Ca_seq in
          [ Asgn (summary_ctr, Add (summary_ctr, Num (-1))) ]
        else [] 
    | _ -> []
  in
  Ca_seq.G.fold_edges_e (fun (from, (stmts, edge_type), to_) ca_rel ->
    let stmts' = (env_counter_stmt edge_type) @ stmts in
    let edge' = (from, (stmts', edge_type), to_) in
    Ca_seq.G.add_edge_e ca_rel edge'
  ) ca Ca_seq.G.empty

let refine_ca_sca_with_env_bounds ca_sca env_bound_map =
  let env_bound_constr edge_type = EnvBoundMap.fold (fun summary_name bound result ->
    let bound = EnvBoundMap.find summary_name env_bound_map in
    let sca_op = match bound with
    | Unbounded -> NonStrict
    | _ -> begin match edge_type with
      | Scfg.S s when s = summary_name -> Strict
      | _ -> NonStrict
    end in
    VariableMap.add (summary_ctr summary_name) sca_op result
  ) env_bound_map VariableMap.empty in
  Ca_sca.G.fold_edges_e (fun (from, (dc, edge_type), to_) ca_rel ->
    let dc' = VariableMap.union (fun _ refined _ -> Some refined) (env_bound_constr edge_type) dc in
    let edge' = (from, (dc', edge_type), to_) in
    G.add_edge_e ca_rel edge'
  ) ca_sca G.empty

module CfgEdge = struct
  type t = Cfg.G.vertex * Scfg.edge_kind * Cfg.G.vertex
  let compare = Pervasives.compare
end
module CfgEdgeMap = Map.Make(CfgEdge)

(** Print edge bound map *)
let print_edge_bound_map =
  CfgEdgeMap.iter (fun (f,et,t) local_bounds ->
    Printf.printf "%s: %s %s\n" (Cfg.G.pprint_cfg_edge (f,([],et),t)) (pprint_bound_asymp local_bounds) (pprint_bound local_bounds)
)

let compute_bound_for_init_heap dot_basename get_edge_color ctx cfg i (init_ca_loc, constraints) =
  Printf.printf "# Computing bounds for initial heap %s\n%!" (Ca_vertex.pprint init_ca_loc) ;

  (* setup dot output *)
  let dot_basename = Printf.sprintf "%s_heap%d" dot_basename i in
  (* let dot_basename = dot_basename^(Ca_vertex.pprint_structure (snd init_ca_loc)) in *)
  let module Ca_seqDot = Ca_seq.G.Dot (struct
    type edge = Ca_seq.G.E.t
    let color_edge = get_edge_color
    let pprint_edge_label (f,(stmts,e),t) = Printf.sprintf "%s\n%s" (Ca_seq.pprint_seq stmts) (Scfg.pprint_edge_kind e)
  end) in
  let module Ca_relDot = Ca_rel.G.Dot (struct
    type edge = Ca_rel.G.E.t
    let color_edge = get_edge_color
    let pprint_edge_label (f,(trel,e),t) = Printf.sprintf "%s\n%s" (Ca_rel.pprint_transrel trel) (Scfg.pprint_edge_kind e)
  end) in
  let module Ca_scaDot = Ca_sca.G.Dot (struct
    type edge = Ca_sca.G.E.t
    let color_edge = get_edge_color
    let pprint_edge_label (f,(trel,e),t) = Printf.sprintf "%s\n%s" (Ca_sca.pprint_transition trel) (Scfg.pprint_edge_kind e)
  end) in

  (* Get lists of summary, effect names from CFG *)
  let summaries, effects = Cfg.G.fold_edges_e (fun (_,(_,summary_ref),_) (summaries, effects) ->
    match summary_ref with
    | Scfg.S name -> VariableSet.add name summaries,                      effects
    | Scfg.E name ->                      summaries, VariableSet.add name effects
  ) cfg (VariableSet.empty, VariableSet.empty) in
  let summaries_and_effects = VariableSet.union summaries effects in

  (* Map summary names to bounds (bounds the summary counter from above) *)
  let env_bound_map = ref (VariableSet.fold (fun summary_name map ->
    let initial_bound = (* initial bound is 0 if there is no CFG edge causing that effect, otherwise unbounded *)
      if VariableSet.mem summary_name effects then Unbounded
      else const_bound_0 ctx
    in
    if summary_name = Scfg.effect_ID_name then map
    else EnvBoundMap.add summary_name initial_bound map
  ) summaries_and_effects EnvBoundMap.empty) in

  (* Build CA from CFG *)
  Debugger.debug "bound" "Building counter automaton from CFG...\n%!" ;
  let ca_seq = Ca_seq.from_cfg cfg init_ca_loc in
  let ca_seq = Ca_seq.propagate_equalities ca_seq in

  (* Set of variables over which the CA is defined *)
  let vars =
    let ca_vars = VariableSet.of_list (Ca_seq.collect_vars ca_seq) in
    let summary_vars = VariableSet.map summary_ctr summaries in
    VariableSet.union ca_vars summary_vars
  in

  (* For bounded CA summary edges, add decrement statement of the summary_counter *)
  let ca_seq = refine_ca_with_env_bounds ca_seq !env_bound_map in
  (* For bounded CA summary edges, refine constraints on initial states, i.e., on summary counters *)
  let refined_constraints = refine_init_constraints_with_env_bounds constraints !env_bound_map in

  let ca_rel = Ca_rel.of_seq ctx ca_seq in
  let ca_sca = Ca_sca.of_rel ctx vars ca_rel in
  Ca_seqDot.write_dot ca_seq dot_basename "seq" ;
  Ca_relDot.write_dot ca_rel dot_basename "rel" ;
  Ca_scaDot.write_dot ca_sca dot_basename "sca" ;

  (* Run interval abstract domain on CA and initial state constraints, to prune infeasible edges. *)
  Debugger.debug "bound" "Removing infeasible edges in CA... %!" ;
  let man, env, abs_map = Ai.do_abstract_computation_initial_values init_ca_loc refined_constraints (VariableSet.elements vars) ca_seq in
  let ca_pruned, num_inf = Ai.remove_infeasible man env abs_map ca_seq init_ca_loc in
  Debugger.debug "bound" "%d pruned\n%!" num_inf ;

  (* let scc_seq = Ca_seq.G.scc_of_cfg_edge ca_pruned 11 0 (Scfg.E "deq_swing") in *)
  (* let scc_seq = Ca_seq.propagate_equalities scc_seq in *)
  (* let scc_rel = Ca_rel.of_seq ctx scc_seq in *)
  (* let scc_sca = Ca_sca.of_rel ctx vars scc_rel in *)
  (* Ca_seqDot.write_dot scc_seq dot_basename "scc_seq" ; *)
  (* Ca_relDot.write_dot scc_rel dot_basename "scc_rel" ; *)
  (* Ca_scaDot.write_dot scc_sca dot_basename "scc_sca" ; *)

  let ca_rel = Ca_rel.of_seq ctx ca_pruned in
  let ca_sca = Ca_sca.of_rel ctx vars ca_rel in
  Ca_seqDot.write_dot ca_pruned dot_basename "seq_pruned" ;
  Ca_relDot.write_dot ca_rel dot_basename "rel_pruned" ;
  Ca_scaDot.write_dot ca_sca dot_basename "sca_pruned" ;
  List.iteri (fun i scc_seq ->
    let scc_rel = Ca_rel.of_seq ctx scc_seq in
    let scc_sca = Ca_sca.of_rel ctx vars scc_rel in
    Ca_relDot.write_dot scc_rel dot_basename (Printf.sprintf "scc_rel_%d" i) ;
    Ca_scaDot.write_dot scc_sca dot_basename (Printf.sprintf "scc_sca_%d" i)
  ) (Ca_seq.G.sccs ca_pruned) ;

  Debugger.info "stats" "CFG: %s, CA: %s, CA (pruned): %s, CA_rel: %s, CA_sca: %s\n" (Cfg.G.pprint_stats cfg) (Ca_seq.G.pprint_stats ca_seq) (Ca_seq.G.pprint_stats ca_pruned) (Ca_rel.G.pprint_stats ca_rel) (Ca_sca.G.pprint_stats ca_sca) ;

  Debugger.debug "bound" "Computing bounds...\n%!" ;

  let cfg_scc_edges = List.concat (Cfg.G.scc_edges cfg) in

  let ca = ref ca_pruned in
  let iteration = ref 1 in
  let result = ref CfgEdgeMap.empty in
  while !iteration > 0 do
    Printf.printf "  Iteration %d, initial state: %s\n%!" !iteration (pprint_env_bound_map ctx !env_bound_map) ;

    let ca_local_bound_map = get_local_bounds ctx vars !ca abs_map in
    let get_ca_local_bounds f t ek = List.fold_left (fun l ((f',ek',t'),lb) ->
      if f'=f && t'=t && ek' = ek then lb :: l else l
    ) [] ca_local_bound_map in

    let edge_bound_map, summary_bounds_map = 
      Cfg.G.fold_edges_e (fun edge (acc_edge_bound_map, acc_env_bound_map) ->
        let f, (_,edge_type), t = edge in
        (* For edge f->t, get a list of local bounds (at most one for each f->t edge in the CA) *)
        let edge_bound =
          (* Check feasibility by checking whether an edge f->t its present in the CA. *)
          match get_ca_local_bounds f t edge_type with
            | [] ->
              (* There is no f->t edge in the CA. Thus f->t is infeasible. *)
              const_bound_0 ctx
            | ca_edge_local_bounds ->
              (* Check if edge belongs to an SCC in the CFG.
               * If not, return constant bound 1. This gets us better constants
               * than testing on the CA; edges may be doubled there because of the
               * refined control structure. *)
              let edge_belongs_to_cfg_scc =
                List.exists (Cfg.G.equal_edge_ignore_labels (f,edge_type,t)) cfg_scc_edges in
              if edge_belongs_to_cfg_scc then
                fold_bounds ctx !env_bound_map ca_edge_local_bounds (f=t)
              else
                const_bound_1 ctx
        in
        (* Debugger.debug "bound" "folded bound for %s: %s\n" (Cfg.pprint_edge edge) (pprint_bound edge_bound) ; *)
        let bound_map = match edge_type with
        | Scfg.E effect_name -> 
            (* the edge is bounded by `bound'; multiply by (N-1) *)
            let add_effect_bound = multiply_by_env_num ctx edge_bound in
            let new_bound_list = match EnvBoundMap.find_opt effect_name acc_env_bound_map with
              | Some bound_list -> add_effect_bound :: bound_list
              | None -> [ add_effect_bound ]
            in
            EnvBoundMap.add effect_name new_bound_list acc_env_bound_map
        | _ -> acc_env_bound_map
        in CfgEdgeMap.add (f,edge_type,t) edge_bound acc_edge_bound_map, bound_map
      ) cfg (CfgEdgeMap.empty, EnvBoundMap.empty)
    in

    (* Combine multiple bounds we found for edges causing effect `summary_name'. *)
    let env_bound_map' = EnvBoundMap.fold (fun summary_name effect_bound_list env_bound_map' ->
      let sum_bound =
        if List.mem Unbounded effect_bound_list then
          Unbounded
        else
          let expr_list = List.map (function Bound e -> e | _ -> assert false) effect_bound_list in
          Bound (Z3.Arithmetic.mk_add ctx expr_list)
      in
      if summary_name = Scfg.effect_ID_name then env_bound_map'
      else EnvBoundMap.add summary_name sum_bound env_bound_map'
    ) summary_bounds_map EnvBoundMap.empty
    in

    (* Merge updated env bound map with previous env bound map.
     * Thus we get env bounds on effects not caused by the current function in
     * the updated map. *)
    let env_bound_map' = EnvBoundMap.merge (fun _ b b' ->
      match (b,b') with _, Some b' -> Some b' | b, None -> b
      ) !env_bound_map env_bound_map'
    in

    (* print_edge_bound_map edge_bound_map ; *)

    (* break if the environment bound map didn't change, i.e., if we reached a fixed point *)
    let env_bound_map_changed = not (EnvBoundMap.equal (=) !env_bound_map env_bound_map') in
    if env_bound_map_changed then (
      iteration := !iteration + 1 ;
      env_bound_map := env_bound_map' ;
      ca := refine_ca_with_env_bounds !ca env_bound_map' ;
      (* Ca_rel.Abstract.Dot.write_dot !ca_sca "rel_abstr.dot" ; *)
    ) else (
      (* quit the loop *)
      iteration := 0;
      result := edge_bound_map

      (* print_edge_bound_map edge_bound_map ; *)
      (* print_newline () ; *)
    )
  done ;
  !result

let get_numeral = Z3.Arithmetic.Integer.get_int
let get_numeral_opt e = if Z3.Expr.is_numeral e then Some (get_numeral e) else None

(** Map any edge to [0] (black). *)
let def_get_color _ = 0

(** [compute_bounds initial_locs_with_constraints cfg] computes bounds on [cfg] with initial locations given by [inital_locs_with_constraints]
 
    [~dot_basename] gives an optional name for outputting [.dot] files.
    [~get_edge_color] defines an optional color map, mapping edge types to colors. *)
let compute_bounds ?(dot_basename="") ?(get_edge_color=def_get_color) init_ca_locs_with_constraints cfg_not_precompiled =
  let cfg = Cfg.precompile cfg_not_precompiled in

  (* Z3 context for constructing expressions *)
  let ctx = Z3.mk_context [] in

  (* Compute bounds for CFG edges per initial heap *)
  let bounds_per_init_heap = List.mapi (compute_bound_for_init_heap dot_basename get_edge_color ctx cfg) init_ca_locs_with_constraints in

  (* For each initial heap, select max bound for each CFG edge *)
  let edge_bound_map = Cfg.G.fold_edges_e (fun (f,(_,et),t) edge_bound_map ->
    let e = f,et,t in
    let bounds = List.map (CfgEdgeMap.find e) bounds_per_init_heap in
    CfgEdgeMap.add e (max_bound ctx bounds) edge_bound_map
  ) cfg CfgEdgeMap.empty in

  print_edge_bound_map edge_bound_map ;

  (* write cfg w/ bounds to file file *)
  let module CfgDot = Cfg.G.Dot (struct
    type edge = Cfg.G.E.t
    let color_edge = get_edge_color
    let pprint_edge_label (f,(stmts,e),t) = 
      let bound = CfgEdgeMap.find (f,e,t) edge_bound_map in
      Printf.sprintf "%s\n%s\n%s\n%s" (pprint_bound_asymp bound) (pprint_bound bound) (Cfg.pprint_seq stmts) (Scfg.pprint_edge_kind e)
  end) in
  CfgDot.write_dot cfg_not_precompiled dot_basename "cfg_bounded" ;
