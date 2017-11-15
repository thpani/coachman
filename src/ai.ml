open Apron
open Ca

(* USAGE: *)
(* let man, env, abs_map = Ai.do_abstract_computation_initial_values init_heaps ca in *)
(* let num_inf = Ai.remove_infeasible man env abs_map ca in *)
(* Printf.printf "Removing infeasible edges (%d)...\n" num_inf ; *)

(* data structure module declarations {{{ *)

module AbsMap = Map.Make(struct
  type t = cloc
  let compare x y = compare (pprint_cloc x) (pprint_cloc y)
end)

let abs_map_equal man a b =
  AbsMap.for_all (fun ploc absv -> match AbsMap.find_opt ploc b with
    | Some absv' -> Abstract1.is_eq man absv absv'
    | None       -> false) a

(* }}} *)

let print_absv man abs_map ca =
  Ca.G.iter_vertex (fun cloc ->
    let open Apron in
    let absv = AbsMap.find cloc abs_map in
    let box = Abstract1.to_box man absv in
    Format.printf "%s %a@." (Ca.pprint_cloc cloc) (Abstract0.print_array Interval.print) box.Apron.Abstract1.interval_array
  ) ca

(* sequential (atomic) abstract execution {{{ *)

let seq_absv man env absv_fploc stmts =
  let rec nexpr_to_expr env e = match e with
    | Id id       -> Texpr1.var env (Var.of_string id)
    | Num num     -> Texpr1.cst env (Coeff.s_of_int num)
    | Add (id, e) -> Texpr1.binop Texpr1.Add (nexpr_to_expr env (Id id)) (nexpr_to_expr env e) Texpr1.Int Texpr1.Near
  in
  let bexpr_to_tcons_array env b = match b with
    | Eq (id, num) | Gt (id, num) | Neg Eq (id, num) ->
      let var = Var.of_string id in
      let expr_var = Texpr1.var env var in
      let expr_num = Texpr1.cst env (Coeff.s_of_int num) in
      let expr_id_minus_num = Texpr1.binop Texpr1.Sub expr_var expr_num Texpr1.Int Texpr1.Near in
      let typ = match b with
      | Eq _ -> Tcons1.EQ | Gt _ -> Tcons1.SUP | Neg Eq _ -> Tcons1.DISEQ
      | _ -> raise (Invalid_argument (Printf.sprintf "Unsupported expression %s" (pprint_bexpr b)))
      in
      let cons = Tcons1.make expr_id_minus_num typ in
      let cons_array = Tcons1.array_make env 1 in
      Tcons1.array_set cons_array 0 cons ; cons_array
    | _ -> raise (Invalid_argument (Printf.sprintf "Unsupported expression %s" (pprint_bexpr b)))
  in
  List.fold_left (fun absv stmt -> 
  let absv' = match stmt with
    | Assume True ->
        absv
  | Assume b ->
      let tcons = bexpr_to_tcons_array env b in
      let cons_absv = Abstract1.of_tcons_array man env tcons in
        Abstract1.meet man absv cons_absv
  | Asgn (id, e) -> 
      let var = Var.of_string id in
      let texpr = nexpr_to_expr env e in
        Abstract1.assign_texpr man absv var texpr None
      absv
  ) absv_fploc stmts

(* }}} *)

(* abstract interpretation fixpoint computation {{{ *)

let do_abstract_computation man env abs_map cfg =
  let vars, _ = Environment.vars env in
  let abs_map = ref abs_map in
  let prev_abs_map = ref AbsMap.empty in
  try begin
    while not (abs_map_equal man !abs_map !prev_abs_map) do
      prev_abs_map := !abs_map ;
      abs_map := G.fold_vertex (fun vertex map ->
          let incoming_absv = G.fold_pred_e (fun (fvertex, (stmts, _), _) l ->
            let absv_fploc = AbsMap.find fvertex !abs_map in
          (absv_seq man env absv_fploc stmts) :: l
        ) cfg vertex [] in
        let absv_list = (AbsMap.find vertex !abs_map) :: incoming_absv in
        let absv = Abstract1.join_array man (Array.of_list absv_list) in
          let widened_itvl_array = Array.map (fun var ->
            let itvl = Abstract1.bound_variable man absv var in
            let testitvl = Interval.of_int 1 3 in
            if Interval.is_leq testitvl itvl then Interval.top else itvl
          ) vars
          in
          let widened_absv = Abstract1.of_box man env vars widened_itvl_array in
          AbsMap.add vertex widened_absv map
      ) cfg !abs_map
    done ;
    (* AbsMap.iter (fun cloc absv -> *)
    (*   let box = (Abstract1.to_box man absv) in *)
    (*   Format.printf "%s %a %a@." (pprint_cloc cloc) (fun x -> Environment.print x) box.Apron.Abstract1.box1_env (Abstract0.print_array Interval.print) box.Apron.Abstract1.interval_array *)
    (* ) !abs_map ; *)
    man, env, !abs_map
  end
  with Manager.Error e -> Printf.eprintf "ERROR: %s; %s\n" e.Apron.Manager.msg (Manager.string_of_funid e.Apron.Manager.funid) ; raise (Manager.Error e)

let do_abstract_computation_initial_values init_clocs vars cfg =
  let man = Box.manager_alloc () in
  let var_array = Array.of_list vars in
  let vars = Array.map Var.of_string var_array in
  let env = Environment.make vars [||] in
  let vars, _ = Environment.vars env in
  let init_interval constr = Array.map (fun v ->
    let var_name = Var.to_string v in
    let init_value = List.assoc_opt var_name constr in
    match init_value with
    | Some i -> i
    | None -> Interval.top
  ) vars in
  let abs_map = G.fold_vertex (fun current_cloc map ->
    let init_heap = List.find_opt (fun (init_cloc, _) -> cloc_equal current_cloc init_cloc) init_clocs in
    let absv = match init_heap with (* check if cloc is an initial vertex *)
    | Some (_, itvl) -> Abstract1.of_box man env vars (init_interval itvl)
    | None -> Abstract1.bottom man env
    in
    AbsMap.add current_cloc absv map
  ) cfg AbsMap.empty
  in
  do_abstract_computation man env abs_map cfg

(* }}} *)

let remove_infeasible man env abs_map cfg =
  let infeasible_edges = G.fold_edges_e (fun e l ->
    let fvertex, (stmts, _), _ = e in
    let absv_fploc = AbsMap.find fvertex abs_map in
    let seq_absv = seq_absv man env absv_fploc stmts in
    if Abstract1.is_bottom man seq_absv then e :: l else l
  ) cfg [] in
  List.iter (fun e -> G.remove_edge_e cfg e) infeasible_edges ;
  List.length infeasible_edges
