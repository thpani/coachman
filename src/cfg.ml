(* stmt type declarations {{{ *)

(* Cfg has a restricted set of statements compared to Ast, i.e., no control statements. *)

type identifier = string

type pexpr =
  | Null
  | Id of identifier
  | Next of identifier

type bexpr =
  | True
  | False
  | Eq of pexpr * pexpr
  | Neg of bexpr

type stmt =
  | Assume of bexpr
  | Alloc of pexpr
  | Asgn of pexpr * pexpr

type seq = stmt list

(* }}} *)

(* pretty printing functions {{{ *)

let pprint_pexpr = function
  | Null    -> "null"
  | Id id   -> id
  | Next id -> id ^ ".next"

let rec pprint_bexpr = function
  | True  -> "true"
  | False -> "false"
  | Eq (a, b) -> Printf.sprintf "(%s) = (%s)" (pprint_pexpr a) (pprint_pexpr b)
  | Neg g     -> Printf.sprintf "!(%s)" (pprint_bexpr g)

let rec pprint_stmt ?(sep=";\n") = function
  | Assume g -> Printf.sprintf "assume(%s)" (pprint_bexpr g)
  | Alloc p      -> Printf.sprintf "%s := new" (pprint_pexpr p)
  | Asgn (a, b)  -> Printf.sprintf "%s := %s" (pprint_pexpr a) (pprint_pexpr b)

and pprint_seq ?(sep=";\n") stmts =
  String.concat sep (List.map (pprint_stmt ~sep:sep) stmts)

(* }}} *)

(* graph module declarations {{{ *)

module G = Scfg.G(struct
  type vertex = Scfg.ploc
  type edge_label = seq

  let compare_vertex = Pervasives.compare
  let hash_vertex    = Hashtbl.hash
  let equal_vertex   = Pervasives.(=)
  let pprint_vertex  = string_of_int
  let get_ploc v     = v

  let compare_edge_label  = Pervasives.compare
  let equal_edge_label    = Pervasives.(=)
  let default_edge_label  = []
end)

(* }}} *)

(* Ast <-> Cfg type conversion {{{ *)

let from_ast_pexpr = function
  | Ast.Null -> Null
  | Ast.Id id -> Id id
  | Ast.Next id -> Next id

let rec from_ast_bexpr = function
  | Ast.True -> True
  | Ast.False -> False
  | Ast.Eq (a, b) -> Eq (from_ast_pexpr a, from_ast_pexpr b)
  | Ast.Neg a -> Neg (from_ast_bexpr a)
  | Ast.CAS _ -> raise (Invalid_argument "CAS should have been rewritten as atomic assume/assign")

let rec from_ast_stmt = List.map (function
  | Ast.Assume b -> Assume (from_ast_bexpr b)
  | Ast.Alloc p -> Alloc (from_ast_pexpr p)
  | Ast.Asgn (a, b) -> Asgn (from_ast_pexpr a, from_ast_pexpr b)
  | s -> raise (Invalid_argument (Printf.sprintf "%s should not appear as CFG stmt" (Ast.pprint_stmt s)))
)

let to_ast_pexpr = function
  | Null -> Ast.Null
  | Id id -> Ast.Id id
  | Next id -> Ast.Next id

let rec to_ast_bexpr = function
  | True  -> Ast.True
  | False -> Ast.False
  | Eq (a, b) -> Ast.Eq (to_ast_pexpr a, to_ast_pexpr b)
  | Neg a ->  Ast.Neg (to_ast_bexpr a)

let rec to_ast_stmt = List.map (function
  | Assume b -> Ast.Assume (to_ast_bexpr b)
  | Alloc p -> Ast.Alloc (to_ast_pexpr p)
  | Asgn (a, b) -> Ast.Asgn (to_ast_pexpr a, to_ast_pexpr b)
)

(* }}} *)

(* AST precompilation {{{ *)

(* This implements the precompilation described in
 * Bouajjani, Bozga, Habermehl, Iosif, Moro, Vojnar:
 * Programs with lists are counter automata. FMSD 38(2): 158-192 (2011)
 *)

(* Each pointer assignment of the form u := new, u := w, or u := w.next is
 * immediately preceded by an assignment of the form u := null. A pointer
 * assignment of the form u := u.next is turned into v := u; u := null; u :=
 * v.next, possibly introducing a fresh variable v. Each pointer assignment of
 * the form u.next := w is immediately preceded by u.next := null.
 *)

(* functions for creating fresh variables *)
let dummy_var = "__l2ca_dummy"
let get_dummy_var s = Printf.sprintf "%s_%s" dummy_var s

(* precompilation procedure *)
let rec precompile_seq stmts = List.concat (List.map precompile_stmt stmts)
and precompile_stmt =
  let precompile_pexpr = function
    | Null    -> [], Null
    | Id id   -> [], Id id
    | Next id -> let d = get_dummy_var id in [ id ], Id d
  in
  let rec precompile_bexpr = function
    | True     -> [], True
    | False    -> [], False
    | Neg g    -> let l, g' = precompile_bexpr g in l, Neg g'
    | Eq (a,b) -> 
      let la, a' = precompile_pexpr a in
      let lb, b' = precompile_pexpr b in
        la @ lb, Eq (a',b')
  in
  function
  | Assume g -> begin
      let id_list, g' = precompile_bexpr g in
      let asgn_dummy_vars = List.map (fun id -> Asgn(Id (get_dummy_var id), Next id)) id_list in
      let null_dummy_vars = List.map (fun id -> Asgn (Id (get_dummy_var id), Null)) id_list in
      (precompile_seq asgn_dummy_vars) @ [ Assume g' ] @ null_dummy_vars
  end
  | Alloc Null                 -> raise (Invalid_argument "null is not a valid lvalue")
  | Alloc (Id id)              -> [ Asgn (Id id, Null) ; Alloc (Id id) ]
  | Alloc (Next _)             -> raise (Invalid_argument "allocation of .next not implemented")
  | Asgn (Null, _)             -> raise (Invalid_argument "null is not a valid lvalue")
  | Asgn (Id id1, Null)        -> [ Asgn (Id id1, Null) ]
  | Asgn (Id id1, Id id2)      -> if id1 <> id2 then [ Asgn (Id id1, Null); Asgn (Id id1, Id id2) ] else []
  | Asgn (Id id1, Next id2)    -> if id1 <> id2 then [ Asgn (Id id1, Null); Asgn (Id id1, Next id2) ] else precompile_seq [ Asgn (Id dummy_var, Id id1); Asgn (Id id1, Next dummy_var); Asgn (Id dummy_var, Null) ]
  | Asgn (Next id1, Null)      -> raise (Invalid_argument "assignment of .next := null not implemented")
  | Asgn (Next id1, Id id2)    -> if id1 <> id2 then [ Asgn (Next id1, Null) ; Asgn (Next id1, Id id2) ] else raise (Invalid_argument "assignment of .next not implemented")
  | Asgn (Next _, Next _)      -> raise (Invalid_argument "assignment of .next := .next not implemented")

let precompile cfg =
  G.fold_edges_e (fun (from, (stmt, summary), to_) acc_g ->
    G.add_edge_e acc_g (from, (precompile_seq stmt, summary), to_)
  ) cfg G.empty

(* }}} *)

(* Create CFG from AST {{{ *)

let from_ast ast = 
  let last = ref 0 in
  let break_target = ref 0 in
  let continue_target = ref 0 in
  let did_goto = ref false in
  let g = G.Imp.create() in
  let max_vertex g = G.Imp.fold_vertex (fun v a -> max v a) g 0 in
  let next_vertex g = (max_vertex g) + 1 in
  let rec gen_cfg ast =
    List.iter (function
      | Ast.IfThenElse (guard, sif, selse) ->
          (* rewrite CAS statements used as conditional *)
          let if_guard, if_guard_effect, else_guard = let open Ast in
          match guard with
            | CAS (a,b,c,d) ->
                Atomic [ Assume (Eq(a, Id b)) ; Asgn(a, Id c) ],
                Scfg.E d,
                Atomic [ Assume(Neg(Eq(a, Id b))) ]
            | _ -> Assume guard, Scfg.effect_ID, Assume (Neg guard)
          in
          let selse = else_guard :: selse in
          let last_before_if = !last in
          begin match if_guard with
            | Ast.Atomic s ->
                let next_v = next_vertex g in
                G.Imp.add_edge_e g (!last, (from_ast_stmt s, if_guard_effect), next_v) ; last := next_v
            | _ -> gen_cfg [if_guard] 
          end ;
          did_goto := false ;
          gen_cfg sif ;
          let did_goto_in_if = !did_goto in
          let last_after_if = !last in
          last := last_before_if ;
          did_goto := false;
          gen_cfg selse ;
          let did_goto_in_else = !did_goto in
          let last_after_else = !last in
          if did_goto_in_if && did_goto_in_else then
            last := !break_target
          else begin
            let next_v = next_vertex g in
            if not did_goto_in_if then
              G.Imp.add_edge_e g (last_after_if, ([Assume True], Scfg.effect_ID), next_v) ;
            if not did_goto_in_else then
              G.Imp.add_edge_e g (last_after_else, ([Assume True], Scfg.effect_ID), next_v) ;
            last := next_v
          end
      | Ast.While(guard, stmts) ->
          let loop_head = !last in
          let loop_exit = next_vertex g in
          let stmts = Ast.Assume(guard) :: stmts in
          break_target := loop_exit ;
          continue_target := loop_head ;
          G.Imp.add_edge_e g (!last, ([Assume(Neg (from_ast_bexpr guard))], Scfg.effect_ID), loop_exit) ;
          gen_cfg stmts ;
          G.Imp.add_edge_e g (!last, ([Assume True], Scfg.effect_ID), loop_head) ;
          last := max_vertex g
      | Ast.Break ->
          Debugger.debug "cfg" "Breaking to %d\n" !break_target ;
          G.Imp.add_edge_e g (!last, ([Assume True], Scfg.effect_ID), !break_target) ;
          did_goto := true
      | Ast.Continue ->
          G.Imp.add_edge_e g (!last, ([Assume True], Scfg.effect_ID), !continue_target) ;
          did_goto := true
      | Ast.Atomic stmt ->
        let next_v = next_vertex g in
        G.Imp.add_edge_e g (!last, (from_ast_stmt stmt, Scfg.effect_ID), next_v) ; last := next_v
      | stmt ->
        let next_v = next_vertex g in
        G.Imp.add_edge_e g (!last, (from_ast_stmt [stmt], Scfg.effect_ID), next_v) ; last := next_v
    ) ast
  in
  gen_cfg ast ;
  (* Fold boolean conditions in assumes *)
  let rec fold_boolean = function
    | True -> True
    | False -> False
    | Neg (Neg s) -> (fold_boolean s)
    | Neg s -> (match fold_boolean s with True -> False | False -> True | _ -> Neg s)
    | s -> s
  in
  G.Imp.iter_edges_e (fun edge ->
    match edge with
    | f, ([Assume s], et), t -> (
      let simplified_s = fold_boolean s in
      G.Imp.remove_edge_e g edge ;
      if simplified_s <> False then G.Imp.add_edge_e g (f, ([Assume simplified_s], et), t)
    )
    | _ -> ()
  ) g ;
  (* Fold assume(true) edges *)
  G.Imp.iter_vertex (fun vertex ->
    match G.Imp.succ_e g vertex with
    | [ _, ([Assume True], et), succ ] -> (
      (* remove the assume true edge *)
      G.Imp.remove_edge_e g (vertex, ([Assume True], et), succ) ;

      (* redirect all predecessor edges to the singleton successor *)
      G.Imp.iter_pred_e (fun (pred, e, _) ->
        G.Imp.remove_edge_e g (pred, e, vertex) ;
        G.Imp.add_edge_e g (pred, e, succ)
      ) g vertex ;

      (* if vertex = 0, rename the successor to 0 *)
      if vertex = 0 then begin
        let vertex_to_rename = succ in
        G.Imp.iter_pred_e (fun (pred, e, _) ->
          G.Imp.remove_edge_e g (pred, e, vertex_to_rename) ;
          G.Imp.add_edge_e g (pred, e, 0)
        ) g vertex_to_rename ;
        G.Imp.iter_succ_e (fun (_, e, succ) ->
          G.Imp.remove_edge_e g (vertex_to_rename, e, succ) ;
          G.Imp.add_edge_e g (0, e, succ)
        ) g succ
      end
    )
    | _ -> () (* not a single successor reachable via assume stmt *)
  ) g ;
  G.remove_unreachable (G.of_imp g) 0

(* }}} *)

(** [add_summaries cfg summaries] constructs the product of the [cfg] and the summary transitions in [summaries]. *)
let add_summaries cfg summaries =
  List.fold_left (fun cfg (summary_name, stmts) ->
    let summary_seq = Ast.unwrap_atomic stmts in
    let summary_nested_list = from_ast_stmt summary_seq in
    G.fold_vertex (fun v cfg ->
      G.add_edge_e cfg (v, (summary_nested_list, Scfg.S summary_name), v)
    ) cfg cfg
  ) cfg summaries
