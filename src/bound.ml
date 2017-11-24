open Ca_sca
open Cavertex
open Debugger
open Util


module Candidate = struct
  type bound = Const of int | Var of Cavertex.VariableSet.t | Unbounded

  let pprint_bound = function 
    | Const i -> string_of_int i
    | Var l -> Cavertex.VariableSet.pprint l
    | Unbounded -> "∞"
  let pprint_bound_factor factor = function
    | Const 0 -> "0"
    | Const 1 -> factor
    | Unbounded -> "∞"
    | bound ->  Printf.sprintf "%s × (%s)" (pprint_bound bound) factor
end

module EnvBoundMap = Map.Make(String)

type bound = Bound of Z3.Expr.expr | Unbounded

let pprint_bound_asymp = let open Z3 in
  let symbol_to_string e = Symbol.get_string (FuncDecl.get_name (Expr.get_func_decl e)) in
  let get_const e =
    if Expr.is_numeral e then
      if (Arithmetic.Integer.get_int e) = 0 then Some "0" else Some "1"
    else if Expr.is_const e then
      Some (symbol_to_string e)
    else
      None
  in
  function
  | Bound e -> Printf.sprintf "O(%s)" (
    (* e is an addition of a numeral, constant, or multiplication of a numeral and constants *)
    match get_const e with
    | Some s -> s
    | None ->
        if Arithmetic.is_add e then
          let args = Expr.get_args e in
          let last_arg = List.nth args ((List.length args) - 1) in
          (* e is in sum-of-monomials format, so this is the highest degree term *)
          match get_const last_arg with
          | Some s -> s
          | None ->
            (* strip out possible constant in multiplication *)
            if Arithmetic.is_mul last_arg then
              let mul_args = Expr.get_args last_arg in
              let constants = match mul_args with
                | first_arg :: tail_arg ->
                    if Expr.is_numeral first_arg then
                      List.fold_left (fun l arg -> (Expr.get_args arg) @ l) [] tail_arg
                    else
                      mul_args
                | [] -> assert false
              in
              let open Cavertex in
              let occ_map = List.fold_left (fun map symbol ->
                let id = symbol_to_string symbol in
                let occ = match VariableMap.find_opt id map with Some i -> i+1 | None -> 1 in
                VariableMap.add id occ map
              ) VariableMap.empty constants
              in
              List.fold_left (fun str (id, occ) ->
                Printf.sprintf "%s%s^%d" str id occ
              ) "" (VariableMap.bindings occ_map)
            else 
              assert false
        else assert false
  )
  | Unbounded -> "∞"

let pprint_bound = function
  | Bound e -> Z3.Expr.to_string (Z3.Expr.simplify e None)
  | Unbounded -> "∞"

let get_local_bounds vars ca =
  let open Cavertex in
  let sccs = Ca_sca.G.scc_edges ca in
  let local_bounds = List.map (fun scc ->
    let var_edge_map = VariableSet.fold (fun var map ->
      let edges_ranked_by_var = List.filter (fun edge -> (* edges on which variable `var' decreases without increasing anywhere in the SCC *)
        let _,(dc,_),_ = edge in
        let decreases_on_edge = (VariableMap.find var dc) = Ca_sca.Strict in
        let increases_in_scc = List.fold_left (fun b (_,(dc,_),_) -> b || ((VariableMap.find var dc) = Ca_sca.DontKnow)) false scc in
        decreases_on_edge && (not increases_in_scc)
      ) scc in
      VariableMap.add var edges_ranked_by_var map
    ) vars VariableMap.empty in
    (* VariableMap.iter (fun var edges_ranked_by_var -> *)
    (*   Printf.printf "Variable %s ranks edges:\n" var ; *)
    (*   List.iter (fun edge -> Printf.printf "  %s" (pprint_edge edge)) edges_ranked_by_var *)
    (* ) var_edge_map ; *)
    List.map (fun edge ->
      (* Debugger.debug "local_bound" "  Edge %s " (Ca_sca.G.pprint_edge edge) ; *)
      (* (2) Let v ∈ V. We define ξ(v) ⊆ E to be the set of all transitions τ = l1 → l2 ∈ E such that v' ≤ v + c ∈ u for some c < 0. For all τ ∈ ξ(v) we set ζ(τ) = v. *)
      let local_bound =
        let ranking_vars = VariableSet.filter (fun var ->
          let edges_ranked_by_var = VariableMap.find var var_edge_map in
          List.exists (Ca_sca.G.equal_edge edge) edges_ranked_by_var
        ) vars in
        if VariableSet.is_empty ranking_vars then begin
          Debugger.debug "local_bound" "ranked by none. " ;
          (* (3) Let v ∈ V and τ ∈ E. Assume τ was not yet assigned a local bound by (1) or (2). We set ζ(τ) = v, if τ does not belong to a strongly connected component (SCC) of the directed graph (L, E′) where E′ = E \ {ξ(v)} (the control flow graph of ∆P without the transitions in ξ(v)). *)
          let ranking_vars = VariableSet.filter (fun var ->
            let edges_ranked_by_var = VariableMap.find var var_edge_map in
            let scc_without_edges_ranked_by_var = List.filter (fun edge ->
              let is_edge_ranked_by_var = List.exists (Ca_sca.G.equal_edge edge) edges_ranked_by_var in
              not is_edge_ranked_by_var
            ) scc in
            let var_breaks_scc = not (Ca_sca.G.edge_in_scc edge scc_without_edges_ranked_by_var) in
            var_breaks_scc
          ) vars in
          if VariableSet.is_empty ranking_vars then begin
            Debugger.debug "local_bound" "Unbounded.\n" ;
            Candidate.Unbounded
          end else begin
            (* Debugger.debug "local_bound" "Vars breaking SCC: %s\n" (Cavertex.VariableSet.to_string ranking_vars) ; *)
            Candidate.Var ranking_vars
          end
        end else begin
          (* Debugger.debug "local_bound" "ranked by %s.\n" (Cavertex.VariableSet.to_string ranking_vars) ; *)
          Candidate.Var ranking_vars
        end
      in edge, local_bound
    ) scc
  ) sccs in
  let local_bounds = List.concat local_bounds in
  Ca_sca.G.fold_edges_e (fun edge l ->
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

let pick_summary_counter_if_possible set =
  (* TODO we prefer summary counters here, because we can bound we know their
   * invairant by construction. In general, we should select the variable with 
   * minimal bound here. *)
  let summary_counters = Cavertex.VariableSet.filter is_summary_ctr set in
  if Cavertex.VariableSet.is_empty summary_counters then
    Cavertex.VariableSet.min_elt set
  else
    Cavertex.VariableSet.min_elt summary_counters

let hitting_set_approx vars =
  (* Select minimal set of variables present in all Var bounds.
  * The optimal solution would be computing the hitting set (NP-complete).
  * We approximate this by
  * (1) checking the intersection over all variable sets.
  * (2) greedily picking one variable from each variable set. *)
  let common_vars = match vars with
    | varset :: _ -> List.fold_left Cavertex.VariableSet.inter varset vars
    | []          -> Cavertex.VariableSet.empty
  in
  if Cavertex.VariableSet.is_empty common_vars then
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
      let c_expr = Util.Z3.mk_numeral ctx c in
      let var_bound_exprs = List.map (function Bound e -> e | Unbounded -> assert false) var_bounds in
      let sum_expr = Z3.Arithmetic.mk_add ctx (c_expr :: var_bound_exprs) in
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

let const_bound ctx i = Bound (Util.Z3.mk_numeral ctx i)
let const_bound_0 ctx = const_bound ctx 0
let const_bound_1 ctx = const_bound ctx 1

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

type var_abs_map = Apron.Interval.t Cavertex.VariableMap.t
type ca_loc_with_constraints = Cavertex.ca_loc * var_abs_map

let refine_init_heaps_with_env_bounds init_heaps env_bound_map =
  List.map (fun (ca_loc, constr) ->
    let constr' = EnvBoundMap.fold (fun summary_name bound acc_constr ->
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
    ) env_bound_map constr in
    ca_loc, constr'
  ) init_heaps

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

let refine_ca_rel_abstract_with_env_bounds ca_rel_abstract env_bound_map =
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
  ) ca_rel_abstract G.empty

let print_edge_bound_map =
  (* print bounds *)
  List.iter (fun ((f,et,t), local_bounds) ->
    Printf.printf "%s: %s %s\n" (Cfg.G.pprint_cfg_edge (f,([],et),t)) (pprint_bound_asymp local_bounds) (pprint_bound local_bounds)
)

let compute_bounds dot_basename get_color (init_heaps : ca_loc_with_constraints list) cfg_not_precompiled =
  let cfg = Cfg.precompile cfg_not_precompiled in

  let summaries, effects = Cfg.G.fold_edges_e (fun (_,(_,summary_ref),_) (summaries, effects) ->
    match summary_ref with
    | Scfg.S name -> VariableSet.add name summaries,                      effects
    | Scfg.E name ->                      summaries, VariableSet.add name effects
  ) cfg (VariableSet.empty, VariableSet.empty) in

  (* Z3 context for constructing expressions *)
  let ctx = Z3.mk_context [] in

  (* Map summary names to bounds (bounds the summary counter from above) *)
  let env_bound_map = ref (VariableSet.fold (fun summary_name map ->
    let initial_bound =
      (* initial bound is 0 if there is no CFG edge causing that effect, otherwise unbounded *)
      if VariableSet.mem summary_name effects then Unbounded
      else const_bound_0 ctx
    in
    EnvBoundMap.add summary_name initial_bound map
  ) summaries EnvBoundMap.empty) in

  Printf.printf "Building CA from CFG...\n%!" ;
  let ca = Ca_seq.from_cfg (List.map fst init_heaps) cfg in

  let vars =
    let ca_vars = VariableSet.of_list (Ca_seq.collect_vars ca) in
    let summary_vars = VariableSet.map summary_ctr summaries in
    VariableSet.union ca_vars summary_vars
  in

  let ca = refine_ca_with_env_bounds ca !env_bound_map in
  let init_heaps = refine_init_heaps_with_env_bounds init_heaps !env_bound_map in

  Printf.printf "Removing infeasible edges in CA... %!" ;
  let man, env, abs_map = Ai.do_abstract_computation_initial_values init_heaps (VariableSet.elements vars) ca in
  let ca, num_inf = Ai.remove_infeasible man env abs_map ca in
  Printf.printf "%d\n%!" num_inf ;

  (* let scc_g = Ca_seq.G.scc_of_cfg_edge ca 11 0 (Scfg.E "deq_swing") in *)
  (* CaDot.write_dot scc_g "scc" "" ; *)

  Printf.printf "Computing relational CA...\n%!" ;
  let ca_rel = Ca_rel.of_ca ctx ca in
  (* Ca_rel.Concrete.Dot.write_dot ca_rel "ca_rel.dot" ; *)

  Printf.printf "Computing SCA CA...\n%!" ;
  let ca_rel_abstract = Ca_sca.of_concrete ctx vars ca_rel in
  let ca_rel_abstract = ref ca_rel_abstract in

  Printf.printf "Computing bounds...\n%!" ;

  let cfg_scc_edges = List.concat (Cfg.scc_edges cfg) in

  let iteration = ref 1 in
  while !iteration > 0 do
    Printf.printf "= Iteration %d, initial state: %s\n%!" !iteration (pprint_env_bound_map !env_bound_map) ;

    let ca_local_bound_map = get_local_bounds vars !ca_rel_abstract in
    let get_ca_local_bounds f t = List.fold_left (fun l ((f',t'),lb) ->
      if f'=f && t'=t then lb :: l else l
    ) [] ca_local_bound_map in

    let edge_bound_map, summary_bounds_map = 
      Cfg.G.fold_edges_e (fun edge (acc_bounds, acc_env_bound_map) ->
        let f, (_,edge_type), t = edge in
        (* For edge f->t, get a list of local bounds (at most one for each f->t edge in the CA) *)
        let edge_bound =
          (* Check feasibility by checking whether an edge f->t its present in the CA. *)
          match get_ca_local_bounds f t with
            | [] ->
              (* There is no f->t edge in the CA. Thus f->t is infeasible. *)
              const_bound_0 ctx
            | ca_edge_local_bounds ->
              (* Check if edge belongs to an SCC in the CFG.
               * If not, return constant bound 1. This gets us better constants
               * than testing on the CA; edges may be doubled there because of the
               * refined control structure. *)
              let edge_belongs_to_cfg_scc = List.mem (f,t) cfg_scc_edges in
              if edge_belongs_to_cfg_scc then
                fold_bounds ctx !env_bound_map ca_edge_local_bounds
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
        in ((f,edge_type,t), edge_bound) :: acc_bounds, bound_map
      ) cfg ([], EnvBoundMap.empty)
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
      EnvBoundMap.add summary_name sum_bound env_bound_map'
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
      ca_rel_abstract := refine_ca_rel_abstract_with_env_bounds !ca_rel_abstract env_bound_map' ;
      (* Ca_rel.Abstract.Dot.write_dot !ca_rel_abstract "rel_abstr.dot" ; *)
    ) else (
      (* quit the loop *)
      iteration := 0;

      print_edge_bound_map edge_bound_map ;
      print_newline () ;

      (* write cfg w/ bounds to file file *)
      (* let module CfgDot = Cfg.Dot (struct *)
      (*   let get_color (_,(_,et),_) = get_color et *)
      (*   let get_label (f,(stmts,e),t) = *) 
      (*     let bound = List.assoc (f,e,t) edge_bound_map in *)
      (*     Printf.sprintf "%s\n%s\n%s" (pprint_bound_asymp bound) (pprint_bound bound) (Cfg.pprint_seq stmts) *)
      (* end) in *)
      (* CfgDot.write_dot cfg_not_precompiled dot_basename "cfg_bounded" ; *)
    )
  done
