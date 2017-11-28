open Apron

open Ca_seq

(* USAGE: *)
(* let man, env, abs_map = Ai.do_abstract_computation_initial_values init_heaps ca in *)
(* let num_inf = Ai.remove_infeasible man env abs_map ca in *)
(* Printf.printf "Removing infeasible edges (%d)...\n" num_inf ; *)

(* data structure module declarations {{{ *)

module VertexMap = Map.Make(struct
  type t = Ca_vertex.ca_loc
  let compare = Ca_vertex.compare
end)

let abs_map_equal man a b =
  VertexMap.for_all (fun ploc absv -> match VertexMap.find_opt ploc b with
    | Some absv' -> Abstract1.is_eq man absv absv'
    | None       -> false) a

(* }}} *)

let print_absv man abs_map ca =
  Ca_seq.G.iter_vertex (fun cloc ->
    let open Apron in
    let absv = VertexMap.find cloc abs_map in
    let box = Abstract1.to_box man absv in
    Format.printf "%s %a@." (Ca_vertex.pprint cloc) (Abstract0.print_array Interval.print) box.Apron.Abstract1.interval_array
  ) ca

(* sequential (atomic) abstract execution {{{ *)

let absv_seq man env absv stmts =
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
  (* 1. For each statement in `stmts', apply a constraint / assignment matching
   * the Assume or Asgn. *)
  let absv = List.fold_left (fun absv stmt -> match stmt with
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
  ) absv stmts in
  (* 2. Bound each counter from below by 0. *)
  let vars, _ = Environment.vars env in
  let array_bounded_by_0 = Lincons1.array_make env (Array.length vars) in
  Array.iteri (fun i var ->
    let linexpr = Linexpr1.make env in
    Linexpr1.set_coeff linexpr var (Coeff.s_of_int 1) ;
    let lincons = Lincons1.make linexpr Lincons1.SUPEQ in
    Lincons1.array_set array_bounded_by_0 i lincons
  ) vars ;
  Abstract1.meet_lincons_array man absv array_bounded_by_0

(* }}} *)

(* Relational abstract execution {{{ *)
let linexpr_of_z3 env ctx expr =
  let open Z3 in
  let params = Params.mk_params ctx in
  Params.add_bool params (Symbol.mk_string ctx "arith_lhs") true ;
  Params.add_bool params (Symbol.mk_string ctx "som") true ;
  let expr = Expr.simplify expr (Some params) in
  let const, vars =
    if Expr.is_const expr then
      let rhs_var = Symbol.get_string (FuncDecl.get_name (Expr.get_func_decl expr)) in
      0, [ 1, rhs_var ]
    else if Expr.is_numeral expr then
      let rhs_num = Arithmetic.Integer.get_int expr in
      rhs_num, []
    else begin
      if not (Arithmetic.is_add expr) then raise (Invalid_argument (Printf.sprintf "Expecting expr of equality to be addition: %s" (Expr.to_string expr))) ;
      let add_args = Expr.get_args expr in
      let const, vars = List.fold_left (fun (const, vars) arg ->
        if Expr.is_const arg then
          const, ( 1, Symbol.get_string (FuncDecl.get_name (Expr.get_func_decl arg)) ) :: vars
        else if Expr.is_numeral arg then
          const + (Arithmetic.Integer.get_int arg), vars
        else begin
          if not (Arithmetic.is_mul arg) then raise (Invalid_argument (Printf.sprintf "Expecting addends to be multiplications: %s" (Expr.to_string arg))) ;
          if not ((Expr.get_num_args arg) = 2) then raise (Invalid_argument (Printf.sprintf "Expecting addends to be linear: %s" (Expr.to_string arg))) ;
          match Expr.get_args arg with
          | [ c ; arg ] -> const, ( Arithmetic.Integer.get_int c, Symbol.get_string (FuncDecl.get_name (Expr.get_func_decl arg)) ) :: vars
          | _ -> assert false
        end
      ) (0, []) add_args in
      const, vars
    end
  in
  let const = Coeff.s_of_int const in
  let vars = List.map (fun (c, v) -> Coeff.s_of_int c, Var.of_string v) vars in
  let linexpr = Linexpr1.make env in
  Linexpr1.set_list linexpr vars (Some const) ; linexpr

let get_primed_var expr = let open Z3 in
  if Expr.is_const expr then
    let symbol = Symbol.get_string (FuncDecl.get_name (Expr.get_func_decl expr)) in
    let symbol_len = String.length symbol in
    let is_primed = symbol.[symbol_len-1] = '\'' in
    if is_primed then
      let unprimed_symbol = String.sub symbol 0 (symbol_len-1) in
      Some (symbol, unprimed_symbol)
    else None
  else None

let absv_rel man env absv (expr, highest_prime) =
  let open Z3 in
  Printf.printf "%s\n" (Expr.to_string expr) ;
  if highest_prime > 1 then invalid_arg "Abstract execution is only implemented for (singly) primed constraints" ;
  let expr = Expr.simplify expr None in
  if not (Boolean.is_and expr) then invalid_arg "Expecting a top-level conjunction" ;
  let constraints = Expr.get_args expr in
  let updates, update_refs, assumptions = List.fold_left (fun (acc_updates, acc_update_refs, acc_assumptions) inequality ->
    (* inequality has either the form 
     * 1. (= x' y')
     * 2. (= x' (lin_expr))
     * 3. (op x lin_expr)
     * which will be accumulated in 1. acc_updates, 2. acc_update_refs, 3. acc_assumptions. *)
    let neg = Boolean.is_not inequality in
    let inner = if neg then List.hd (Expr.get_args inequality) else inequality in
    let args = Expr.get_args inner in
    let lhs = List.nth args 0 in
    if not (Expr.is_const lhs) then invalid_arg (Printf.sprintf "Expecting LHS of inequality to be const: %s" (Expr.to_string inequality)) ;
    let ctx = mk_context [] in
    match get_primed_var lhs with
    | Some (lhs_symbol, lhs_unprimed_symbol) -> (
      if not (Boolean.is_eq inequality) then invalid_arg (Printf.sprintf "Expecting update constraint to be equality: %s" (Expr.to_string inequality)) ;
      let rhs = List.nth args 1 in
      match get_primed_var rhs with
      | Some (_,rhs_unprimed_symbol) ->
          acc_updates, (lhs_unprimed_symbol, rhs_unprimed_symbol) :: acc_update_refs, acc_assumptions
      | None ->
          let linexpr = linexpr_of_z3 env ctx rhs in
          (lhs_unprimed_symbol, linexpr) :: acc_updates, acc_update_refs, acc_assumptions
    )
    | None -> (
      let params = Params.mk_params ctx in
      Params.add_bool params (Symbol.mk_string ctx "arith_lhs") true ;
      Params.add_bool params (Symbol.mk_string ctx "som") true ;
      let inequality = Expr.simplify inequality (Some params) in
      let neg = Boolean.is_not inequality in
      let inner = if neg then List.hd (Expr.get_args inequality) else inequality in
      let comparison =
        if (not neg) && Boolean.is_eq inner then Lincons1.EQ
        else if Arithmetic.is_le inner then Lincons1.SUP
        else if ((not neg) && (Arithmetic.is_ge inner)) then Lincons1.SUPEQ
        else invalid_arg (Printf.sprintf "Expecting inequality in normal form: %s" (Expr.to_string inequality)) (* should not happen after Z3 simplification *)
      in
      let args = Expr.get_args inner in
      let lhs = List.nth args 0 in
      let linexpr = linexpr_of_z3 env ctx lhs in
      assert (Coeff.equal_int (Linexpr1.get_cst linexpr) 0) ;
      let rhs = List.nth args 1 in
      let cst = Arithmetic.Integer.get_int rhs in
      Linexpr1.set_cst linexpr (Coeff.s_of_int (-cst)) ;
      if neg && (comparison = Lincons1.SUP) then ((* flip coefficients *)
        let vars, _ = Environment.vars env in
        Array.iter (fun var -> Linexpr1.set_coeff linexpr var (Coeff.neg (Linexpr1.get_coeff linexpr var))) vars
      ) ;
      let cons = Lincons1.make linexpr comparison in
      let cons_array = Lincons1.array_make env 1 in
      Lincons1.array_set cons_array 0 cons ;
      acc_updates, acc_update_refs, cons_array :: acc_assumptions
    )
  ) ([], [], []) constraints in
  let absv = List.fold_left (fun acc_absv assumption -> 
    Abstract1.meet_lincons_array man acc_absv assumption
  ) absv assumptions in
  let absv = List.fold_left (fun acc_absv (var_name, linexpr) ->
    let var = Var.of_string var_name in
    Abstract1.assign_linexpr man acc_absv var linexpr None
  ) absv updates in
  let absv = List.fold_left (fun acc_absv (var_name, update_ref) ->
    let var = Var.of_string var_name in
    let linexpr = List.assoc update_ref updates in
    Abstract1.assign_linexpr man acc_absv var linexpr None
  ) absv update_refs in
  absv

(* }}} *)

(* abstract interpretation fixpoint computation {{{ *)

let do_abstract_computation man env abs_map cfg =
  let vars, _ = Environment.vars env in
  let abs_map = ref abs_map in
  let prev_abs_map = ref VertexMap.empty in
  try begin
    while not (abs_map_equal man !abs_map !prev_abs_map) do
      prev_abs_map := !abs_map ;
      abs_map := G.fold_vertex (fun vertex map ->
        let incoming_absv = G.fold_pred_e (fun (fvertex, (stmts, _), _) l ->
          let absv_fploc = VertexMap.find fvertex !abs_map in
          (absv_seq man env absv_fploc stmts) :: l
        ) cfg vertex [] in
        let absv_list = (VertexMap.find vertex !abs_map) :: incoming_absv in
        let absv = Abstract1.join_array man (Array.of_list absv_list) in
        let widened_itvl_array = Array.map (fun var ->
          let itvl = Abstract1.bound_variable man absv var in
          let testitvl = Interval.of_int 1 3 in
          if Interval.is_leq testitvl itvl then Interval.top else itvl
        ) vars
        in
        let widened_absv = Abstract1.of_box man env vars widened_itvl_array in
        VertexMap.add vertex widened_absv map
      ) cfg !abs_map
    done ;
    man, env, !abs_map
  end
  with Manager.Error e -> Printf.eprintf "ERROR: %s; %s\n" e.Apron.Manager.msg (Manager.string_of_funid e.Apron.Manager.funid) ; raise (Manager.Error e)

let do_abstract_computation_initial_values init_ca_loc constraints vars cfg =
  (* let pprint_constr = List.iter (fun (var, itvl) -> *)
  (*   Format.printf "%s |-> %a@." var Interval.print itvl *)
  (* ) in *)
  (* Printf.printf "Initial intervals:\n" ; *)
  (* List.iter (fun (cloc, constr) -> *)
  (*   Printf.printf "%s" (pprint_cloc cloc); *)
  (*   pprint_constr constr ; *)
  (*   print_newline () *)
  (* ) init_clocs ; *)
  let man = Box.manager_alloc () in
  let var_array = Array.of_list vars in
  let vars = Array.map Var.of_string var_array in
  let env = Environment.make vars [||] in
  let vars, _ = Environment.vars env in
  let init_interval constr = Array.map (fun v ->
    let var_name = Var.to_string v in
    let init_value = Ca_vertex.VariableMap.find_opt var_name constr in
    match init_value with
    | Some i -> i
    | None -> Interval.top
  ) vars in
  let abs_map = G.fold_vertex (fun current_cloc map ->
    let absv =
      if Ca_vertex.equal current_cloc init_ca_loc then
        Abstract1.of_box man env vars (init_interval constraints)
      else
        Abstract1.bottom man env
    in
    VertexMap.add current_cloc absv map
  ) cfg VertexMap.empty
  in
  do_abstract_computation man env abs_map cfg

(* }}} *)

let remove_infeasible man env abs_map cfg initv =
  let g_wo_inf_edges, num_inf = G.fold_edges_e (fun e (acc_g,acc_removed) ->
    let fvertex, (stmts, _), _ = e in
    let absv  = VertexMap.find fvertex abs_map in
    let absv' = absv_seq man env absv stmts in
    if Abstract1.is_bottom man absv' then
      acc_g, acc_removed+1
    else
      G.add_edge_e acc_g e, acc_removed
  ) cfg (G.empty, 0) in
  G.remove_unreachable g_wo_inf_edges initv, num_inf
