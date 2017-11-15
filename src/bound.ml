open Ca
open Ca_rel.Abstract
open Debugger
open Util

let scc_edges g = 
  let scc_list = SCC.scc_list g in
  List.map (fun scc_vertices ->
    (* for this SCC... *)
    List.fold_left (fun l scc_vertex_from ->
      (* and this vertex, get all edges to successors that are also in the same SCC *)
      let scc_vertex_from_succs_in_scc = List.fold_left (fun l edge ->
        let _, _, to_ = edge in
        let to_in_same_scc = List.exists (cloc_equal to_) scc_vertices in
        if to_in_same_scc then edge :: l else l
      ) [] (G.succ_e g scc_vertex_from)
      in
      scc_vertex_from_succs_in_scc @ l
    ) [] scc_vertices
  ) scc_list

module Candidate = struct
  type bound = Const of int | Var of StringSet.t | Unbounded

  let pprint_bound = function 
    | Const i -> string_of_int i
    | Var l -> StringSet.to_string l
    | Unbounded -> "∞"
  let pprint_bound_factor factor = function
    | Const 0 -> "0"
    | Const 1 -> factor
    | Unbounded -> "∞"
    | bound ->  Printf.sprintf "%s × (%s)" (pprint_bound bound) factor
end

module EnvBoundMap = Map.Make(String)

type bound = Bound of Z3.Expr.expr | Unbounded
let pprint_bound = function
| Bound e -> Z3.Expr.to_string e
| Unbounded -> "∞"

let pprint_edge (f,_,t) = Printf.sprintf "%s -> %s" (pprint_cloc f) (pprint_cloc t)

let edge_equal (c1,e1,c1') (c2,e2,c2') =
  cloc_equal c1 c2 && cloc_equal c1' c2' && e1 = e2

let edge_in_scc edge scc =
  let g = List.fold_left G.add_edge_e G.empty scc in
  let scc_edges = List.concat (scc_edges g) in
  List.exists (edge_equal edge) scc_edges

let get_local_bounds vars ca =
  let sccs = scc_edges ca in
  let local_bounds = List.map (fun scc ->
    let var_edge_map = StringSet.fold (fun var map ->
      let edges_ranked_by_var = List.filter (fun edge -> (* edges on which variable `var' decreases without increasing anywhere in the SCC *)
        let _,(dc,_),_ = edge in
        let decreases_on_edge = (StringMap.find var dc) = Strict in
        let increases_in_scc = List.fold_left (fun b (_,(dc,_),_) -> b || ((StringMap.find var dc) = DontKnow)) false scc in
        decreases_on_edge && (not increases_in_scc)
      ) scc in
      StringMap.add var edges_ranked_by_var map
    ) vars StringMap.empty in
    (* StringMap.iter (fun var edges_ranked_by_var -> *)
    (*   Printf.printf "Variable %s ranks edges:\n" var ; *)
    (*   List.iter (fun edge -> Printf.printf "  %s" (pprint_edge edge)) edges_ranked_by_var *)
    (* ) var_edge_map ; *)
    List.map (fun edge ->
      Debugger.logf Debugger.Info "bound" "  Edge %s " (pprint_edge edge) ;
      (* (2) Let v ∈ V. We define ξ(v) ⊆ E to be the set of all transitions τ = l1 → l2 ∈ E such that v' ≤ v + c ∈ u for some c < 0. For all τ ∈ ξ(v) we set ζ(τ) = v. *)
      let local_bound =
        let ranking_vars = StringSet.filter (fun var ->
          let edges_ranked_by_var = StringMap.find var var_edge_map in
          List.exists (edge_equal edge) edges_ranked_by_var
        ) vars in
        if StringSet.is_empty ranking_vars then begin
          Debugger.logf Debugger.Info "bound" "ranked by none. " ;
          (* (3) Let v ∈ V and τ ∈ E. Assume τ was not yet assigned a local bound by (1) or (2). We set ζ(τ) = v, if τ does not belong to a strongly connected component (SCC) of the directed graph (L, E′) where E′ = E \ {ξ(v)} (the control flow graph of ∆P without the transitions in ξ(v)). *)
          let ranking_vars = StringSet.filter (fun var ->
            let edges_ranked_by_var = StringMap.find var var_edge_map in
            let scc_without_edges_ranked_by_var = List.filter (fun edge ->
              let is_edge_ranked_by_var = List.exists (edge_equal edge) edges_ranked_by_var in
              not is_edge_ranked_by_var
            ) scc in
            let var_breaks_scc = not (edge_in_scc edge scc_without_edges_ranked_by_var) in
            var_breaks_scc
          ) vars in
          if StringSet.is_empty ranking_vars then begin
            Debugger.logf Debugger.Info "bound" "Unbounded.\n" ;
            Candidate.Unbounded
          end else begin
            Debugger.logf Debugger.Info "bound" "Vars breaking SCC: %s\n" (StringSet.to_string ranking_vars) ;
            Candidate.Var ranking_vars
          end
        end else begin
          Debugger.logf Debugger.Info "bound" "ranked by %s.\n" (StringSet.to_string ranking_vars) ;
          Candidate.Var ranking_vars
        end
      in edge, local_bound
    ) scc
  ) sccs in
  let local_bounds = List.concat local_bounds in
  G.fold_edges_e (fun edge l ->
    let (f,_),_,(t,_) = edge in
    let local_bound = match List.assoc_opt edge local_bounds with
    | Some lb -> lb
    | None -> Candidate.Const 1
    in
    ((f,t),local_bound) :: l
  ) ca []

let summary_ctr summary_name = Printf.sprintf "summary_ctr_%s" summary_name
let is_summary_ctr id = Str.string_match (Str.regexp "^summary_ctr_\\(.*\\)") id 0
let get_summary_of_summary_ctr id =
  if is_summary_ctr id then Some (Str.matched_group 1 id)
  else None

let pprint_env_bound_map map = String.concat "; " (List.map (fun (summary_name, bound) ->
  Printf.sprintf "%s = %s" (summary_ctr summary_name) (pprint_bound bound)
) (EnvBoundMap.bindings map))

let get_refined_env_bounds effect_name env_bounds = function
  | []     -> raise (Invalid_argument "Expect at least one local bound; infeasible edges should be assigned Const 0.")
  | [ lb ] -> begin match lb with
    | Candidate.Const i -> EnvBoundMap.add effect_name lb env_bounds
    | _ -> env_bounds (* TODO bounded by variable *)
  end
  | _ -> env_bounds (* TODO multiple abstract edges with different ranking functions *)

let hitting_set_approx vars =
  (* Select minimal set of variables present in all Var bounds.
  * The optimal solution for sum-of-vars would be computing the hitting set (NP-complete).
  * We approximate this by
  * (1) checking the intersection over all variable sets.
  * (2) greedily picking one variable from each variable set. *)
  let common_vars = match vars with
    | varset :: _ -> List.fold_left StringSet.inter varset vars
    | []          -> StringSet.empty
  in
  if StringSet.is_empty common_vars then
    List.map pick_summary_counter_if_possible vars
  else
    [ pick_summary_counter_if_possible common_vars ]

let fold_bounds ctx env_bound_map bounds =
  let c, unbounded, vars = List.fold_left (fun (const_carry, unbounded_carry, var_carry) bound ->
    let open Candidate in match bound with
    | Const i   -> const_carry + i, unbounded_carry, var_carry
    | Unbounded -> const_carry    , true           , var_carry
    | Var  vars -> const_carry    , unbounded_carry, vars :: var_carry
  ) (0,false,[]) bounds in
  if unbounded then Unbounded
  else
    let vars = hitting_set_approx vars in
    let var_bounds = List.map (fun var ->
      (* TODO bound other variables *)
      match get_summary_of_summary_ctr var with
      | Some summary -> EnvBoundMap.find summary env_bound_map
      | None -> Unbounded
    ) vars in
    if List.mem Unbounded var_bounds then
      Unbounded
    else
      let c_expr = Ca_rel.mk_numeral ctx c in
      let var_bound_exprs = List.map (function Bound e -> e | Unbounded -> assert false) var_bounds in
      let sum_expr = Z3.Arithmetic.mk_add ctx (c_expr :: var_bound_exprs) in
      Bound (Z3.Expr.simplify sum_expr None)

let multiply_by_env_num ctx = function
  | Unbounded  -> Unbounded
  | Bound expr ->
      let n = Ca_rel.mk_const ctx 0 "N" in
      let one = Ca_rel.mk_numeral ctx 1 in
      let env_num = Z3.Arithmetic.mk_sub ctx [ n ; one ] in
      let mult_expr = Z3.Arithmetic.mk_mul ctx [ expr ; env_num ] in
      Bound (Z3.Expr.simplify mult_expr None)

let const_bound ctx i = Bound (Ca_rel.mk_numeral ctx i)
let const_bound_0 ctx = const_bound ctx 0
let const_bound_1 ctx = const_bound ctx 1

let refine_ca_with_env_bounds ca_rel_abstract env_bound_map =
  let env_bound_constr edge_type = EnvBoundMap.fold (fun summary_name bound result ->
    let bound = EnvBoundMap.find summary_name env_bound_map in
    let sca_op = match bound with
    | Unbounded -> NonStrict
    | _ -> begin match edge_type with
      | Cfg.S s when s = summary_name -> Strict
      | _ -> NonStrict
    end in
    StringMap.add (summary_ctr summary_name) sca_op result
  ) env_bound_map StringMap.empty in
  Ca_rel.Abstract.G.fold_edges_e (fun (from, (dc, edge_type), to_) ca_rel ->
    let dc' = StringMap.union (fun _ refined _ -> Some refined) (env_bound_constr edge_type) dc in
    let edge' = (from, (dc', edge_type), to_) in
    G.add_edge_e ca_rel edge'
  ) ca_rel_abstract G.empty

let compute_bounds dot_basename get_color init_heaps cfg_not_precompiled =
  let cfg = Cfg.precompile cfg_not_precompiled in

  let summaries, effects = Cfg.G.fold_edges_e (fun (_,(_,summary_ref),_) (summaries, effects) ->
    match summary_ref with
    | Cfg.S name -> StringSet.add name summaries,                    effects
    | Cfg.E name ->                    summaries, StringSet.add name effects
  ) cfg (StringSet.empty, StringSet.empty) in

  (* Z3 context for constructing expressions *)
  let ctx = Z3.mk_context [] in

  (* Map summary names to bounds (bounds the summary counter from above) *)
  let env_bound_map = ref (StringSet.fold (fun summary_name map ->
    let initial_bound =
      (* initial bound is 0 if there is no CFG edge causing that effect, otherwise unbounded *)
      if StringSet.mem summary_name effects then Unbounded
      else const_bound_0 ctx
    in
    EnvBoundMap.add summary_name initial_bound map
  ) summaries EnvBoundMap.empty) in

  let vars =
    let ca_vars = StringSet.of_list (Ca.collect_vars ca) in
    let summary_vars = StringSet.map summary_ctr summaries in
    StringSet.union ca_vars summary_vars
  in

  Printf.printf "Abstracting...\n%!" ;
  let ca_rel_abstract = Ca_rel.Abstract.of_ca ca in
  let ca_rel_abstract = refine_ca_with_env_bounds ca_rel_abstract !env_bound_map in
  let ca_rel_abstract = ref ca_rel_abstract in
  Ca_rel.Abstract.Dot.write_dot !ca_rel_abstract "rel_abstr.dot" ;

  Printf.printf "Computing bounds...\n%!" ;

  let cfg_scc_edges = List.concat (Cfg.scc_edges cfg) in

  let running = ref true in
  while !running do
    Printf.printf "= Start iteration, initial state: %s\n%!" (pprint_env_bound_map !env_bound_map) ;

    let ca_local_bound_map = get_local_bounds vars !ca_rel_abstract in
    let get_ca_local_bounds f t = List.fold_left (fun l ((f',t'),lb) ->
      if f'=f && t'=t then lb :: l else l
    ) [] ca_local_bound_map in

    let edge_bound_map = Cfg.G.fold_edges_e (fun (f, (_,edge_type), t) result ->
      (* For edge f->t, get a list of local bounds (at most one for each f->t edge in the CA) *)
      let local_bounds =
        let ca_edge_local_bounds = get_ca_local_bounds f t in
        (* Check feasibility by checking whether an edge f->t its present in the CA. *)
        match ca_edge_local_bounds with
          | [] ->
            (* There is no f->t edge in the CA. Thus f->t is infeasible. *)
            const_bound_0 ctx
          | _  ->
            (* Check if edge belongs to an SCC in the CFG.
             * If not, return constant bound 1. This gets us better constants
             * than testing on the CA; edges may be doubled there because of the
             * refined control structure. *)
            let edge_belongs_to_cfg_scc = List.mem (f,t) cfg_scc_edges in
            if edge_belongs_to_cfg_scc then fold_bounds ctx !env_bound_map ca_edge_local_bounds
            else const_bound_1 ctx
      in ((f,edge_type,t),local_bounds) :: result
    ) cfg [] in

    let env_bound_map' = EnvBoundMap.fold (fun summary_name _ map ->
      (* find the edge causing effect `summary_name' *)
      let edge_bound_map_of_summary = List.filter (fun ((f,edge_type,t), local_bounds) ->
        match edge_type with
        | Cfg.E effect_name -> effect_name = summary_name
        | _ -> false
      ) edge_bound_map in
      let lb = match edge_bound_map_of_summary with
      | []           -> const_bound_0 ctx (* there is no edge causing this effect *)
      | [ _, bound ] -> multiply_by_env_num ctx bound (* the edge is bounded by `bound'; multiply by (N-1) *)
      | _            -> assert false (* there must not be >1 edges causing one specific effect *)
      in EnvBoundMap.add summary_name lb map
    ) !env_bound_map EnvBoundMap.empty in

    List.iter (fun ((f,edge_type,t), local_bounds) ->
      Printf.printf "%d -> %d (%s):\t%s\n" f t (Cfg.pprint_edge_type edge_type) (pprint_bound local_bounds) ;
    ) edge_bound_map ;
    Printf.printf "\n%!" ;

    (* break if the environment bound map didn't change, i.e., if we reached a fixed point *)
    let env_bound_map_changed = not (EnvBoundMap.equal (=) !env_bound_map env_bound_map') in
    if env_bound_map_changed then begin
      env_bound_map := env_bound_map' ;
      ca_rel_abstract := refine_ca_with_env_bounds !ca_rel_abstract env_bound_map' ;
      Ca_rel.Abstract.Dot.write_dot !ca_rel_abstract "rel_abstr.dot" ;
    end else
      running := false
    ;
  done
