open Ca
open Ca_rel.Abstract

open Debugger

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

type bound = Const of int | Var of identifier list | Unbounded

let pprint_bound = function 
  | Const i -> string_of_int i
  | Var l -> begin match l with
    | [ var ] -> var
    | l -> "one of: {" ^ (String.concat ", " l) ^ "}"
  end
  | Unbounded -> "∞"
let pprint_bound_factor factor = function
  | Const 0 -> "0"
  | Const 1 -> factor
  | Unbounded -> "∞"
  | bound ->  Printf.sprintf "%s × (%s)" (pprint_bound bound) factor
let pprint_bound_list l = String.concat " + " (List.map pprint_bound l)

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
    let var_edge_list = List.map (fun var ->
      let edges_ranked_by_var = List.filter (fun edge -> (* edges on which variable `var' decreases without increasing anywhere in the SCC *)
        let _,(dc,_),_ = edge in
        let decreases_on_edge = (List.assoc var dc) = Strict in
        let increases_in_scc = List.fold_left (fun b (_,(dc,_),_) -> b || ((List.assoc var dc) = DontKnow)) false scc in
        decreases_on_edge && (not increases_in_scc)
      ) scc in
      var, edges_ranked_by_var
    ) vars in
    (* List.iter (fun (var, edges_ranked_by_var) -> *)
    (*   Printf.printf "Variable %s ranks edges:\n" var ; *)
    (*   List.iter (fun edge -> Printf.printf "  %s" (pprint_edge edge)) edges_ranked_by_var *)
    (* ) var_edge_list ; *)
    List.map (fun edge ->
      Debugger.logf Debugger.Info "bound" "  Edge %s " (pprint_edge edge) ;
      (* (2) Let v ∈ V. We define ξ(v) ⊆ E to be the set of all transitions τ = l1 → l2 ∈ E such that v' ≤ v + c ∈ u for some c < 0. For all τ ∈ ξ(v) we set ζ(τ) = v. *)
      let local_bound =
        let ranking_vars = List.filter (fun var ->
          let edges_ranked_by_var = List.assoc var var_edge_list in
          List.exists (edge_equal edge) edges_ranked_by_var
        ) vars in
        if (List.length ranking_vars) > 0 then begin
          Debugger.logf Debugger.Info "bound" "ranked by %s.\n" (String.concat ", " ranking_vars) ;
          Var ranking_vars
        end else begin
          Debugger.logf Debugger.Info "bound" "ranked by none. " ;
          (* (3) Let v ∈ V and τ ∈ E. Assume τ was not yet assigned a local bound by (1) or (2). We set ζ(τ) = v, if τ does not belong to a strongly connected component (SCC) of the directed graph (L, E′) where E′ = E \ {ξ(v)} (the control flow graph of ∆P without the transitions in ξ(v)). *)
          let ranking_vars = List.filter (fun var ->
            let edges_ranked_by_var = List.assoc var var_edge_list in
            let scc_without_edges_ranked_by_var = List.filter (fun edge ->
              let is_edge_ranked_by_var = List.exists (edge_equal edge) edges_ranked_by_var in
              not is_edge_ranked_by_var
            ) scc in
            let var_breaks_scc = not (edge_in_scc edge scc_without_edges_ranked_by_var) in
            var_breaks_scc
          ) vars in
          match ranking_vars with
          | [] -> Debugger.logf Debugger.Info "bound" "Unbounded.\n" ; Unbounded
          | _  -> Debugger.logf Debugger.Info "bound" "Vars breaking SCC: %s\n" (String.concat ", " ranking_vars) ; Var ranking_vars
        end
      in edge, local_bound
    ) scc
  ) sccs in
  let local_bounds = List.concat local_bounds in
  G.fold_edges_e (fun edge l ->
    let (f,_),_,(t,_) = edge in
    let local_bound = match List.assoc_opt edge local_bounds with
    | Some lb -> lb
    | None -> Const 1
    in
    ((f,t),local_bound) :: l
  ) ca []

module EnvBoundMap = Map.Make(String)
module VarSet = Set.Make(String)

let summary_ctr summary_name = Printf.sprintf "summary_ctr_%s" summary_name

let pprint_env_bound_map map = String.concat "; " (List.map (fun (summary_name, bound) ->
  Printf.sprintf "%s = %s" (summary_ctr summary_name) (pprint_bound_factor "N-1" bound)
) (EnvBoundMap.bindings map))

let get_refined_env_bounds effect_name env_bounds = function
  | []     -> raise (Invalid_argument "Expect at least one local bound; infeasible edges should be assigned Const 0.")
  | [ lb ] -> begin match lb with
    | Const i -> EnvBoundMap.add effect_name lb env_bounds
    | _ -> env_bounds (* TODO bounded by variable *)
  end
  | _ -> env_bounds (* TODO multiple abstract edges with different ranking functions *)

let fold_bounds bounds =
  let c, unbounded, vars = List.fold_left (fun (const_carry, unbounded_carry, var_carry) bound ->
    match bound with
    | Const i   -> const_carry + i, unbounded_carry, var_carry
    | Unbounded -> const_carry    , true           , var_carry
    | Var  vars -> const_carry    , unbounded_carry, (VarSet.of_list vars) :: var_carry
  ) (0,false,[]) bounds in
  if unbounded then [ Unbounded ]
  else match vars with
  | [] -> [ Const c ]
  | _ ->
    (* The optimal solution for sum-of-vars would be computing the hitting set (NP-complete).
     * We approximate this by
     * (1) checking the intersection over all variable sets.
     * (2) greedily picking one variable from each variable set. *)
    let common_vars = match vars with
      | varset :: tail -> List.fold_left VarSet.inter varset vars
      | []             -> VarSet.empty
    in
    let some_vars = List.sort_uniq compare (List.map (fun vars -> Var [VarSet.min_elt vars]) vars) in
    let vars =
      if VarSet.is_empty common_vars then some_vars
      else [Var (VarSet.elements common_vars)] in
      if c > 0 then (Const c) :: vars else vars

let refine_ca_with_env_bounds ca_rel_abstract env_bound_map =
  let env_bound_constr edge_type = EnvBoundMap.fold (fun summary_name bound result ->
    let bound = EnvBoundMap.find summary_name env_bound_map in
    let sca_op = match bound with
    | Unbounded -> NonStrict
    | _ -> begin match edge_type with
      | Cfg.S s when s = summary_name -> Strict
      | _ -> NonStrict
    end in
    (summary_ctr summary_name, sca_op) :: result
  ) env_bound_map [] in
  Ca_rel.Abstract.G.fold_edges_e (fun (from, (dc, edge_type), to_) ca_rel ->
    let dc' = (env_bound_constr edge_type) @ dc in
    let edge' = (from, (dc', edge_type), to_) in
    G.add_edge_e ca_rel edge'
  ) ca_rel_abstract G.empty

let compute_bounds cfg ca =
  let summaries, effects = Cfg.G.fold_edges_e (fun e (summaries, effects) ->
    let _, (_,summary_ref), _ = e in
    match summary_ref with
    | Cfg.S id -> id :: summaries, effects
    | Cfg.E name -> summaries, name :: effects
  ) cfg ([], []) in
  let summaries, effects = List.sort_uniq compare summaries, List.sort_uniq compare effects in
  (* summary_name |-> bound *)
  let env_bound_map = ref (List.fold_left (fun map summary_name ->
    let initial_bound =
      (* initial bound is 0 if there is no CFG edge causing that effect, otherwise unbounded *)
      if List.mem summary_name effects then Unbounded
      else Const 0
    in
    EnvBoundMap.add summary_name initial_bound map
  ) EnvBoundMap.empty summaries) in

  let vars = (Ca.collect_vars ca) @ (List.map summary_ctr summaries) in

  Printf.printf "Abstracting...\n%!" ;
  let ca_rel_abstract = Ca_rel.Abstract.of_ca ca in
  let ca_rel_abstract = refine_ca_with_env_bounds ca_rel_abstract !env_bound_map in
  let ca_rel_abstract = ref ca_rel_abstract in
  Printf.printf "%!" ;
  Ca_rel.Abstract.Dot.write_dot !ca_rel_abstract "rel_abstr.dot" ;


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
        let ca_edge_local_bounds = fold_bounds (get_ca_local_bounds f t) in
        (* Check feasibility by checking whether an edge f->t its present in the CA. *)
        match ca_edge_local_bounds with
          | [] -> [ Const 0 ]  (* There is no f->t edge in the CA. Thus f->t is infeasible. *)
          | _  ->
            (* Check if edge does not belong to an SCC in the CFG.
             * This gets us better constants than testing on the CA; edges may be 
             * doubled there because of the refined control structure. *)
            let edge_belongs_to_cfg_scc = List.mem (f,t) cfg_scc_edges in
            if edge_belongs_to_cfg_scc then ca_edge_local_bounds
            else [ Const 1 ]
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
      | []            -> Const 0 (* there is no edge causing this effect *)
      | [ _, bounds ] -> begin match bounds with
        | [ bound ] -> bound
        | _ -> Unbounded
      end
      | _             -> assert (false) (* there must not be >1 edges causing one specific effect *)
      in EnvBoundMap.add summary_name lb map
    ) !env_bound_map EnvBoundMap.empty in

    List.iter (fun ((f,edge_type,t), local_bounds) ->
      Printf.printf "%d -> %d (%s): %s\n" f t (Cfg.pprint_edge_type edge_type) (pprint_bound_list local_bounds) ;
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
