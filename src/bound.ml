open Ca_sca
open Ca_vertex
open Debugger
open Util

module VariableSet = Util.DS.IdentifierSet
module VariableMap = Util.DS.IdentifierMap
module EnvBoundMap = Util.DS.IdentifierMap

type var_abs_map = Apron.Interval.t VariableMap.t
type ca_loc_with_constraints = Ca_vertex.ca_loc * var_abs_map

module Rank = struct
  type t = Const of int | Var of VariableSet.t * int option VariableMap.t | Unbounded
end

type t = Bound of Z3.Expr.expr | Unbounded

let pprint = function
  | Bound e ->
      let ctx = Config.get_ctx () in
      let params = Z3.Params.mk_params ctx in
      Z3.Params.add_bool params (Z3.Symbol.mk_string ctx "som") true ;
      Z3.Expr.to_string (Z3.Expr.simplify e (Some params))
  | Unbounded -> "∞"

let const_bound ctx i = Bound (Util.Z3.mk_numeral ctx i)
let const_bound_0 ctx = const_bound ctx 0
let const_bound_1 ctx = const_bound ctx 1

(* Conversion to asymptotic complexity {{{ *)

let to_complexity = function
  | Unbounded -> Complexity.Unbounded
  | Bound e -> Complexity.of_z3_expr e

let pprint_bound_asymp b = Complexity.pprint (to_complexity b)

(* }}} *)

(* Simplify max/min ite expressions {{{ *)

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

let min_bound ctx b b' =
  match b, b' with
  | Unbounded, _ -> b'
  | _, Unbounded -> b
  | Bound e1, Bound e2 -> Bound (min_expr ctx e1 e2)

(* }}} *)

(* Invariants on upper bound variables {{{ *)

(** [itvl_width itvl] returns [None] if the interval is bottom, otherwise [Some w] where [w] is [upper(itvl)-lower(itvl)]. *)
let itvl_width itvl =
  let lower, upper = itvl.Apron.Interval.inf, itvl.Apron.Interval.sup in
  if (Apron.Scalar.is_infty lower) = 1 || (Apron.Scalar.is_infty upper) = -1 then
    Some 0 (* infeasible *)
  else if (Apron.Scalar.is_infty lower) = (-1) || (Apron.Scalar.is_infty upper) = 1 then
    None (* unbounded *)
  else
    (* extract greatest interval *)
    let lower = match lower with Apron.Scalar.Mpqf i -> i | _ -> assert false in
    let upper = match upper with Apron.Scalar.Mpqf i -> i | _ -> assert false in
    let lower = int_of_string (Mpqf.to_string lower) in
    let upper = int_of_string (Mpqf.to_string upper) in
    Some (upper - lower)

(** [get_ub_invariant var scc abs_map] returns the greatest width of intervals
  * in the domain of [abs_map], mapping vertices in [scc] to abstract values. *)
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

(** [get_ub_invariant_vars ranking_vars scc abs_map] returns a map from
  * variables in [ranking_vars] to greatest interval widths according to
  * [abs_map] on vertices from [scc]. *)
let get_ub_invariant_vars ranking_vars scc abs_map =
  VariableSet.fold (fun v ub_map ->
    VariableMap.add v (get_ub_invariant v scc abs_map) ub_map
  ) ranking_vars VariableMap.empty

(* }}} *)

let ca_edge_eq (f,(_,k),t) (f',k',t') = k = k' && Ca_vertex.equal f f' && Ca_vertex.equal t t'

let get_local_bounds ctx man env vars ca_seq abs_map_ca (init_ca_loc,init_abs_map) =
  Debugger.debug "local_bound" "  Computing size-change abstraction and edges ranked by variables\n%!" ;
  let map_var_ranked_edges = Hashtbl.create (VariableSet.cardinal vars) in
  let map_edge_ranking_vars = Ca_sca.G.EdgeHashtbl.create (Ca_seq.G.nb_edges ca_seq) in
  List.iteri (fun i scc_seq ->
    Debugger.debug "local_bound_ranked" "    SCC %d\n%!" i ;
    Debugger.debug "local_bound_ranked" "      %s\n%!" (Ca_seq.G.pprint_stats scc_seq) ;
    Debugger.debug "local_bound_ranked" "      Propagating equalities\n%!" ;
    (* ATTENTION: this is sound only because we do this per SSC *)
    let scc_seq = Ca_seq.propagate_equalities scc_seq in
    Debugger.debug "local_bound_ranked" "      Computing SCA of seq CA\n%!" ;
    let scc_sca = Ca_sca.of_seq ctx vars scc_seq in
    Debugger.debug "local_bound_ranked" "      Computing map edge -> ranking vars\n%!" ;
    VariableSet.iter (fun var ->
      Ca_sca.G.iter_edges_e (fun (f,(dc,k),t) ->
        (* edges on which variable `var' decreases without increasing anywhere in the SCC *)
        let decreases_on_edge = (VariableMap.find var dc) = Ca_sca.Strict in
        let increases_in_scc = Ca_sca.G.fold_edges_e (fun (_,(dc,_),_) b -> b || ((VariableMap.find var dc) = Ca_sca.DontKnow)) scc_sca false in
        if decreases_on_edge && (not increases_in_scc) then begin
          Hashtbl.add map_var_ranked_edges var (f,k,t) ;
          (* TODO Assumption: v -ek-> v' is unique *)
          Ca_sca.G.EdgeHashtbl.add map_edge_ranking_vars (f,k,t) var
        end
      ) scc_sca
    ) vars
  ) (Ca_seq.G.sccs ca_seq) ;

  Debugger.debug "local_bound" "  Computing map edge -> in SCC?\n%!" ;
  let map_edge_in_scc = Ca_seq.G.EdgeHashtbl.create (Ca_seq.G.nb_edges ca_seq) in
  List.iter (fun (f,(_,k),t) -> Ca_seq.G.EdgeHashtbl.add map_edge_in_scc (f,k,t) true) (List.concat (Ca_seq.G.scc_edges ca_seq)) ;

  Debugger.debug "local_bound" "  Computing map edge -> in SCC after removing var-ranked edges?\n%!" ;
  let map_edge_var_in_scc_w_varranked_removed = Ca_seq.G.EdgeHashtbl.create (Ca_seq.G.nb_edges ca_seq) in
  VariableSet.iter (fun var -> 
    let varranked_edges = Hashtbl.find_all map_var_ranked_edges var in
    let ca_seq_wo_varranked_edges = Ca_seq.G.filter_edge_e (fun edge ->
        not (List.exists (ca_edge_eq edge) varranked_edges)
      ) ca_seq
    in
    let ca_seq_wo_varranked_edges = if !Config.use_ai then
        begin
          Debugger.debug "local_bound" "    Pruning infeasible edges after removing edges ranked by variable `%s'\n%!" var ;
          let abs_map = Ai.do_abstract_computation man env (Some init_abs_map) ca_seq_wo_varranked_edges in
          let ca_seq_wo_varranked_edges_pruned, inf = Ai.remove_infeasible man env abs_map ca_seq_wo_varranked_edges (Some init_ca_loc) in
          ca_seq_wo_varranked_edges_pruned
        end
      else
        ca_seq_wo_varranked_edges
    in
    List.iter (fun scc ->
        List.iter (fun (f,(_,k),t) -> (* an edge of an scc after removing var-ranked edges *)
            Ca_seq.G.EdgeHashtbl.add map_edge_var_in_scc_w_varranked_removed (f,k,t) var
          ) scc
      ) (Ca_seq.G.scc_edges ca_seq_wo_varranked_edges)
  ) vars ;

  Debugger.debug "local_bound" "  Computing map edge -> SCC\n%!" ;
  let map_edge_scc = Ca_seq.G.EdgeHashtbl.create (Ca_seq.G.nb_edges ca_seq) in
  List.iter (fun scc_edges ->
      let scc = List.fold_left (fun g edge -> Ca_seq.G.add_edge_e g edge) Ca_seq.G.empty scc_edges in
      List.iter (fun (f,(_,k),t) ->
          Ca_seq.G.EdgeHashtbl.add map_edge_scc (f,k,t) scc
        ) scc_edges
    ) (Ca_seq.G.scc_edges ca_seq) ;

(*   Debugger.debug "local_bound" "  Computing map var -> ub_invariant\n%!" ; *)
(*   let map_var_ub_invariant = Hashtbl.create (VariableSet.cardinal vars) in *)
(*   VariableSet.iter (fun var -> *)
(*       get_ub_invariant var scc abs_map *)
(*     ) vars ; *)

  Debugger.debug "local_bound" "  Computing local bounds\n%!" ;
  Ca_seq.G.fold_edges_e (fun edge map ->
    Debugger.debug "local_bound_edge" "  Edge %s %!" (Ca_seq.G.pprint_edge edge) ;
    let local_bound =
      (* (1) We set ζ(τ) = 1 for all transitions τ that do not belong to an SCC. *)
      let edge_wo_label = let f,(_,k),t = edge in f,k,t in
      if not (Ca_seq.G.EdgeHashtbl.mem map_edge_in_scc edge_wo_label) then
        begin
          Debugger.debug "local_bound_edge" "outside SCC. Const.\n" ;
          Rank.Const 1
        end
      else
        let f,(_,ek),t = edge in
        let ranking_vars = Ca_sca.G.EdgeHashtbl.find_all map_edge_ranking_vars (f,ek,t) in
        let ranking_vars = List.fold_right VariableSet.add ranking_vars VariableSet.empty in
        if not (VariableSet.is_empty ranking_vars) then
          (* (2) Let v ∈ V. We define ξ(v) ⊆ E to be the set of all transitions τ = l1 → l2 ∈ E such that v' ≤ v + c ∈ u for some c < 0. For all τ ∈ ξ(v) we set ζ(τ) = v. *)
          begin
            let f,(_,k),t = edge in
            let scc_seq = Ca_seq.G.EdgeHashtbl.find map_edge_scc (f,k,t) in
            let inv_ranking_vars = get_ub_invariant_vars ranking_vars scc_seq abs_map_ca in
            Debugger.debug "local_bound_edge" "ranked by %s.\n" (VariableSet.pprint ranking_vars) ;
            Rank.Var (ranking_vars, inv_ranking_vars)
          end
        else
          begin
            Debugger.debug "local_bound_edge" "ranked by none. " ;
            (* (3) Let v ∈ V and τ ∈ E. Assume τ was not yet assigned a local bound by (1) or (2). We set ζ(τ) = v, if τ does not belong to a strongly connected component (SCC) of the directed graph (L, E′) where E′ = E \ {ξ(v)} (the control flow graph of ∆P without the transitions in ξ(v)). *)
            let ranking_vars = 
              let non_ranking_vars = Ca_seq.G.EdgeHashtbl.find_all map_edge_var_in_scc_w_varranked_removed edge_wo_label in
              let non_ranking_vars_set = List.fold_right VariableSet.add non_ranking_vars VariableSet.empty in
              VariableSet.diff vars non_ranking_vars_set
            in
            if not (VariableSet.is_empty ranking_vars) then
              begin
                let f,(_,k),t = edge in
                let scc_seq = Ca_seq.G.EdgeHashtbl.find map_edge_scc (f,k,t) in
                let inv_ranking_vars = get_ub_invariant_vars ranking_vars scc_seq abs_map_ca in
                Debugger.debug "local_bound_edge" "Vars breaking SCC%s: %s\n" (if !Config.use_ai then " with AI pruning" else "") (VariableSet.pprint ranking_vars) ;
                Rank.Var (ranking_vars, inv_ranking_vars)
              end
            else
              begin
                Debugger.debug "local_bound_edge" "Unbounded.\n" ;
                Rank.Unbounded
              end
          end
    in
    let (f,_),(_,k),(t,_) = edge in
    ((f,k,t), local_bound) :: map
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
  Printf.sprintf "%s = %s" (summary_ctr summary_name) (pprint bound)
) (EnvBoundMap.bindings map))

let minimal_hitting_set_approx var_sets =
  (* Select minimal set of variables present in all Var bounds.
   * The exact solution would be to compute the hitting set (NP-complete, Karp'72)
   * We implement a partial solution by checking the union and intersection over
   * all variable sets for solutions.
   * Otherwise, we approximate the minimal hitting set as union of all variable
   * sets.
   * *)
  List.iter (fun var_set -> assert ((VariableSet.cardinal var_set) > 0)) var_sets ;
  match var_sets with
  | [] -> (* no edges ranked by vars, hitting set is empty*)
      VariableSet.empty
  | var_set :: _ ->
    let intersection = List.fold_left VariableSet.inter var_set var_sets in
    if not (VariableSet.is_empty intersection) then
      intersection
    else
      let one_ele_union = List.fold_left (fun one_ele_union var_set ->
        if (VariableSet.cardinal var_set) = 1 then
          VariableSet.union one_ele_union var_set
        else
          one_ele_union
      ) VariableSet.empty var_sets
      in
      List.iter (fun var_set ->
        let intersection = VariableSet.inter one_ele_union var_set in
        if (VariableSet.is_empty intersection) then print_endline (List.pprint VariableSet.pprint var_sets) ;
        assert (not (VariableSet.is_empty intersection))
      ) var_sets ;
      one_ele_union

let max_map_element list_of_maps key =
  List.fold_left (fun max_width map ->
    let width = VariableMap.find key map in
    match (max_width, width) with
    | None, _ -> None
    | _, None -> None
    | Some i, Some j -> Some (max i j)
  ) (Some 0) list_of_maps

let variable_bound ctx env_bound_map inv self_loop var =
  let bound = match get_summary_of_summary_ctr var with
  | Some summary ->
      (* var is an environment counter, by construction we can look its
        * upper bound invariant up in the env bound map. *)
      EnvBoundMap.find summary env_bound_map
  | None ->
      (* var is a CA counter, get its upper bound invariant from the
        * interval abstract interpretation pass. *)
      match VariableMap.find var inv with
      | Some c -> Bound (Z3.mk_numeral ctx c)
      | None -> Unbounded
  in
  (* if the transition is not a self-loop, add +1 for edges *before* the test *)
  match (bound, self_loop) with
  | Bound e, true -> Bound e
  | Bound e, false -> Bound (Z3.Arithmetic.mk_add ctx [ e ; (Z3.mk_numeral ctx 1)])
  | Unbounded, _ -> Unbounded

let min_bound_vars ctx env_bound_map self_loop var_set inv =
  VariableSet.fold (fun var min ->
    let bound = variable_bound ctx env_bound_map inv self_loop var in
    min_bound ctx min bound
  ) var_set Unbounded

let fold_bounds ctx env_bound_map local_bounds_per_scc self_loop =
  let const, unbounded, var_sets, invs =
    List.fold_left (fun (const, unbounded, var_sets, invs) bound ->
      let open Rank in match bound with
      | Const i            -> const + i, unbounded, var_sets           , invs
      | Unbounded          -> const    , true     , var_sets           , invs
      | Var (var_set, inv) -> const    , unbounded, var_set :: var_sets, inv :: invs
    ) (0,false,[],[]) local_bounds_per_scc in
  if unbounded then Unbounded
  else
    let bound_list = List.map2 (min_bound_vars ctx env_bound_map self_loop) var_sets invs in
    if List.mem Unbounded bound_list then
      Unbounded
    else
      let c_expr = Util.Z3.mk_numeral ctx const in
      let var_bounds = List.map (function Bound b -> b | _ -> assert false) bound_list in
      let sum_expr = Z3.Arithmetic.mk_add ctx (c_expr :: var_bounds) in
      Bound (Z3.Expr.simplify sum_expr None)

let multiply_by_env_num ctx = function
  | Unbounded  -> Unbounded
  | Bound expr ->
      let open Util in
      let n = Z3.mk_const ctx "N" in
      let one = Z3.mk_numeral ctx 1 in
      let env_num = Z3.Arithmetic.mk_sub ctx [ n ; one ] in
      let mult_expr = Z3.Arithmetic.mk_mul ctx [ expr ; env_num ] in
      Bound (Z3.Expr.simplify mult_expr None)

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
          let bound = Util.Z3.get_int expr in
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
type cfg_edge_map = t CfgEdgeMap.t

(** Print edge bound map *)
let print_edge_bound_map map =
  CfgEdgeMap.iter (fun (f,et,t) local_bounds ->
      match et with
      | Scfg.E _ ->
        Debugger.info "bound" "  %s: %s %s\n" (Cfg.G.pprint_cfg_edge (f,et,t)) (pprint_bound_asymp local_bounds) (pprint local_bounds)
      | _ -> ()
    ) map ;
   CfgEdgeMap.iter (fun (f,et,t) local_bounds ->
      match et with
      | Scfg.S _ ->
        Debugger.info "bound" "  %s: %s %s\n" (Cfg.G.pprint_cfg_edge (f,et,t)) (pprint_bound_asymp local_bounds) (pprint local_bounds)
      | _ -> ()
  ) map

let compute_bound_for_init_heap get_edge_color ctx cfg i (init_ca_loc, constraints) =
  Debugger.info "bound" "Computing bounds for initial heap %s\n%!" (Ca_vertex.pprint init_ca_loc) ;

  (* setup dot output *)
  (* let dot_basename = Printf.sprintf "%s_heap%d" !Config.dot_basename i in *)
  (* let dot_basename = dot_basename^(Ca_vertex.pprint_structure (snd init_ca_loc)) in *)
  (* let module Ca_seqDot = Ca_seq.G.Dot (struct *)
  (*   type edge = Ca_seq.G.E.t *)
  (*   type vertex = Ca_vertex.ca_loc *)
  (*   let pprint_vertex = Ca_vertex.Vertex.pprint_vertex *)
  (*   let color_edge = get_edge_color *)
  (*   let pprint_edge_label (f,(stmts,e),t) = Printf.sprintf "%s\n%s" (Ca_seq.pprint_seq stmts) (Scfg.pprint_edge_kind e) *)
  (* end) in *)
  (* let module Ca_relDot = Ca_rel.G.Dot (struct *)
  (*   type edge = Ca_rel.G.E.t *)
  (*   type vertex = Ca_vertex.ca_loc *)
  (*   let pprint_vertex = Ca_vertex.Vertex.pprint_vertex *)
  (*   let color_edge = get_edge_color *)
  (*   let pprint_edge_label (f,(trel,e),t) = Printf.sprintf "%s\n%s" (Ca_rel.pprint_transrel trel) (Scfg.pprint_edge_kind e) *)
  (* end) in *)
  (* let module Ca_scaDot = Ca_sca.G.Dot (struct *)
  (*   type edge = Ca_sca.G.E.t *)
  (*   type vertex = Ca_vertex.ca_loc *)
  (*   let pprint_vertex = Ca_vertex.Vertex.pprint_vertex *)
  (*   let color_edge = get_edge_color *)
  (*   let pprint_edge_label (f,(trel,e),t) = Printf.sprintf "%s\n%s" (Ca_sca.pprint_transition trel) (Scfg.pprint_edge_kind e) *)
  (* end) in *)

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
  let ca_seq = Ca_seq.of_cfg cfg init_ca_loc in
  Debugger.debug "bound" "  %s\n%!" (Ca_seq.G.pprint_stats ca_seq) ;

  (* Debugger.debug "bound" "Propagating equalities...\n%!" ; *)
  (* let ca_seq = Ca_seq.propagate_equalities ca_seq in *)

  (* Set of variables over which the CA is defined *)
  let vars =
    let ca_vars = VariableSet.of_list (Ca_seq.collect_vars ca_seq) in
    let summary_vars = VariableSet.map summary_ctr summaries in
    VariableSet.union ca_vars summary_vars
  in
  Debugger.debug "bound" "  counter variables: %s\n" (VariableSet.pprint vars) ;

  (* For bounded CA summary edges, add decrement statement of the summary_counter *)
  let ca_seq = refine_ca_with_env_bounds ca_seq !env_bound_map in
  (* For bounded CA summary edges, refine constraints on initial states, i.e., on summary counters *)
  let refined_constraints = refine_init_constraints_with_env_bounds constraints !env_bound_map in

  (* let ca_rel = Ca_rel.of_seq ctx ca_seq in *)
  (* let ca_sca = Ca_sca.of_rel ctx vars ca_rel in *)
  (* Ca_seqDot.write_dot ca_seq dot_basename "seq" ; *)
  (* Ca_relDot.write_dot ca_rel dot_basename "rel" ; *)
  (* Ca_scaDot.write_dot ca_sca dot_basename "sca" ; *)

  (* Run interval abstract domain on CA and initial state constraints, to prune infeasible edges. *)
  Debugger.debug "bound" "Pruning infeasible edges in CA...\n%!" ;
  let man, env, init_abs_map = Ai.get_init_absv init_ca_loc refined_constraints (VariableSet.elements vars) ca_seq in
  let abs_map = Ai.do_abstract_computation man env (Some init_abs_map) ca_seq in
  (* print_absv man env init_ca_loc (VertexMap.find init_ca_loc abs_map) ; *)
  (* print_abs_map man env abs_map cfg ; *)
  let ca_pruned, num_inf = Ai.remove_infeasible man env abs_map ca_seq (Some init_ca_loc) in
  (* Ai.print_abs_map man env abs_map ca_pruned ; *)
  Debugger.debug "bound" "  %s\n%!" (Ca_seq.G.pprint_stats ca_pruned) ;

  (* let module Ca_seqDot = Ca_seq.G.Dot (struct *)
  (*   type edge = Ca_seq.G.E.t *)
  (*   type vertex = Ca_vertex.ca_loc *)
  (*   let pprint_vertex v = "\"" ^ (Ca_vertex.pprint v) ^ "\n" ^ (Ai.pprint_absv man (Ai.VertexMap.find v abs_map)) ^ "\"" *)
  (*   let color_edge = get_edge_color *)
  (*   let pprint_edge_label (f,(stmts,e),t) = Printf.sprintf "%s\n%s" (Ca_seq.pprint_seq stmts) (Scfg.pprint_edge_kind e) *)
  (* end) in *)
  (* let module Ca_relDot = Ca_rel.G.Dot (struct *)
  (*   type edge = Ca_rel.G.E.t *)
  (*   type vertex = Ca_vertex.ca_loc *)
  (*   let pprint_vertex v = "\"" ^ (Ca_vertex.pprint v) ^ "\n" ^ (Ai.pprint_absv man (Ai.VertexMap.find v abs_map)) ^ "\"" *)
  (*   let color_edge = get_edge_color *)
  (*   let pprint_edge_label (f,(trel,e),t) = Printf.sprintf "%s\n%s" (Ca_rel.pprint_transrel trel) (Scfg.pprint_edge_kind e) *)
  (* end) in *)
  (* let module Ca_scaDot = Ca_sca.G.Dot (struct *)
  (*   type edge = Ca_sca.G.E.t *)
  (*   type vertex = Ca_vertex.ca_loc *)
  (*   let pprint_vertex v = "\"" ^ (Ca_vertex.pprint v) ^ "\n" ^ (Ai.pprint_absv man (Ai.VertexMap.find v abs_map)) ^ "\"" *)
  (*   let color_edge = get_edge_color *)
  (*   let pprint_edge_label (f,(trel,e),t) = Printf.sprintf "%s\n%s" (Ca_sca.pprint_transition trel) (Scfg.pprint_edge_kind e) *)
  (* end) in *)

  (* Debugger.debug "bound" "Abstracting CA...\n%!" ; *)
  (* let ca_rel = Ca_rel.of_seq ctx ca_pruned in *)
  (* let ca_sca = Ca_sca.of_rel ctx vars ca_rel in *)
  (* Ca_seqDot.write_dot ca_seq dot_basename "ca_seq" ; *)
  (* Ca_seqDot.write_dot ca_pruned dot_basename "ca_seq_pruned" ; *)
  (* Ca_relDot.write_dot ca_rel dot_basename "rel_pruned" ; *)
  (* Ca_scaDot.write_dot ca_sca dot_basename "sca_pruned" ; *)
  (* List.iteri (fun i scc_seq -> *)
    (* let scc_rel = Ca_rel.of_seq ctx scc_seq in *)
    (* let scc_sca = Ca_sca.of_rel ctx vars scc_rel in *)
    (* Ca_seqDot.write_dot scc_seq dot_basename (Printf.sprintf "scc_seq_%d" i) ; *)
    (* Ca_relDot.write_dot scc_rel dot_basename (Printf.sprintf "scc_rel_%d" i) ; *)
    (* Ca_scaDot.write_dot scc_sca dot_basename (Printf.sprintf "scc_sca_%d" i) *)
  (* ) (Ca_seq.G.sccs ca_pruned) ; *)

  Debugger.debug "bound" "Computing bounds...\n%!" ;

  let cfg_scc_edges = List.concat (Cfg.G.scc_edges cfg) in

  let ca = ref ca_pruned in
  let iteration = ref 1 in
  let result = ref CfgEdgeMap.empty in
  while !iteration > 0 do
    Debugger.info "bound" "  Iteration %d, initial state: %s\n%!" !iteration (pprint_env_bound_map ctx !env_bound_map) ;

    let ca_local_bound_map = get_local_bounds ctx man env vars !ca abs_map (init_ca_loc, init_abs_map) in
    Debugger.debug "bound" "  Accumulating local bounds\n%!" ;
    let get_ca_local_bounds f t ek = List.fold_left (fun l ((f',ek',t'),lb) ->
      if f'=f && t'=t && ek' = ek then lb :: l else l
    ) [] ca_local_bound_map in

    let edge_bound_map, summary_bounds_map =
      Cfg.G.fold_edges_e (fun edge (acc_edge_bound_map, acc_env_bound_map) ->
        (* Debugger.debug "bound" "    Folding bound for %s:\n" (Cfg.G.pprint_edge edge) ; *)
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
                List.exists (fun (f',(_,edge_type'),t') -> Cfg.G.equal_edge_ignore_labels (f,edge_type,t) (f',edge_type',t')) cfg_scc_edges in
              if edge_belongs_to_cfg_scc then
                fold_bounds ctx !env_bound_map ca_edge_local_bounds (f=t)
              else
                const_bound_1 ctx
        in
        (* Debugger.debug "bound" "folded bound for %s: %s\n" (Cfg.G.pprint_edge edge) (pprint edge_bound) ; *)
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
    iteration := !iteration + 1 ;
    if env_bound_map_changed then
      begin
        env_bound_map := env_bound_map' ;
        ca := refine_ca_with_env_bounds !ca env_bound_map' ;
      end
    else
      (* fixpoint. break out of loop. *)
      begin
        (* quit the loop *)
        iteration := 0;
        result := edge_bound_map

        (* print_edge_bound_map edge_bound_map ; *)
        (* print_newline () ; *)
      end
  done ;
  !result

let get_numeral_opt e = if Z3.Expr.is_numeral e then Some (Util.Z3.get_int e) else None

(** Map any edge to [0] (black). *)
let default_get_color _ = 0

(** [compute_bounds initial_locs_with_constraints cfg] computes bounds on [cfg] with initial locations given by [inital_locs_with_constraints]

    [~get_edge_color] defines an optional color map, mapping edge types to colors. *)
let compute_bounds ?(get_edge_color=default_get_color) init_ca_locs_with_constraints cfg_not_precompiled =
  let cfg = Cfg.precompile cfg_not_precompiled in
  let module CfgDot = Cfg.G.Dot (struct
      type edge = Cfg.G.E.t
      type vertex = Cfg.ploc
      let pprint_vertex = string_of_int
      let color_edge = get_edge_color
      let pprint_edge_label (f,(stmts,e),t) =
        Printf.sprintf "%s\n%s" (Cfg.pprint_seq stmts) (Scfg.pprint_edge_kind e)
    end) in
  CfgDot.write_dot cfg_not_precompiled !Config.dot_basename "cfg" ;

  let ctx = Config.get_ctx () in

  (* Compute bounds for CFG edges per initial heap *)
  let bounds_per_init_heap = List.mapi (compute_bound_for_init_heap get_edge_color ctx cfg) init_ca_locs_with_constraints in

  (* For each initial heap, select max bound for each CFG edge *)
  let edge_bound_map = Cfg.G.fold_edges_e (fun (f,(_,et),t) edge_bound_map ->
    let e = f,et,t in
    let bounds = List.map (CfgEdgeMap.find e) bounds_per_init_heap in
    CfgEdgeMap.add e (max_bound ctx bounds) edge_bound_map
  ) cfg CfgEdgeMap.empty in

  edge_bound_map

let write_bound_dot edge_bound_map get_edge_color cfg_not_precompiled =
  (* write cfg w/ bounds to file file *)
  let module CfgDot = Cfg.G.Dot (struct
    type edge = Cfg.G.E.t
    type vertex = Cfg.ploc
    let pprint_vertex = string_of_int
    let color_edge = get_edge_color
    let pprint_edge_label (f,(stmts,e),t) =
      let bound = CfgEdgeMap.find (f,e,t) edge_bound_map in
      Printf.sprintf "%s\n%s\n%s\n%s" (pprint_bound_asymp bound) (pprint bound) (Cfg.pprint_seq stmts) (Scfg.pprint_edge_kind e)
  end) in
  CfgDot.write_dot cfg_not_precompiled !Config.dot_basename "cfg_bounded" ;
