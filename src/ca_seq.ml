open Ca_vertex

(* This implements
 *  Ahmed Bouajjani, Marius Bozga, Peter Habermehl, Radu Iosif, Pierre Moro, Tomás Vojnar:
 *  Programs with lists are counter automata. Formal Methods in System Design 38(2): 158-192 (2011)
 *
 * On top, we add support for atomic transitions composed of multiple commands.
 *) 

(* type declarations {{{ *)

type identifier = string

type nexpr =
  | Id of identifier
  | Num of int
  | Add of identifier * nexpr

type bexpr =
  | True
  | False
  | Eq of identifier * int
  | Gt of identifier * int
  | Neg of bexpr

type stmt =
  | Assume of bexpr
  | Asgn of identifier * nexpr

type program = (identifier * stmt list) list

(* }}} *)

(* pretty printing functions {{{ *)

let rec pprint_nexpr = function
  | Id id -> id
  | Num n -> string_of_int n
  | Add (id, e) -> Printf.sprintf "%s + (%s)" id (pprint_nexpr e)

let rec pprint_bexpr = function
  | True  -> "true"
  | False -> "false"
  | Eq (id, e) -> Printf.sprintf "(%s) = (%s)" id (string_of_int e)
  | Gt (id, e) -> Printf.sprintf "(%s) > (%s)" id (string_of_int e)
  | Neg g     -> Printf.sprintf "!(%s)" (pprint_bexpr g)

let rec pprint_stmt ?(sep=";\n") ?(atomic_angles=true) = function
  | Assume g      -> Printf.sprintf "assume(%s)" (pprint_bexpr g)
  | Asgn (id, e)  -> Printf.sprintf "%s := %s" id (pprint_nexpr e)

and pprint_seq ?(sep=";\n") ?(atomic_angles=true) stmts =
  String.concat sep (List.map (pprint_stmt ~sep ~atomic_angles) stmts)

(* }}} *)

(* graph module declarations {{{ *)

module G = Scfg.G(struct
  include Vertex

  type edge_label = stmt list

  let compare_edge_label  = Pervasives.compare
  let equal_edge_label    = Pervasives.(=)
  let default_edge_label  = []
end)

(* }}} *)

let ctr_of_node n = Printf.sprintf "x_%d" n
let tmp_ctr_of_node n = Printf.sprintf "tmp_x_%d" n

let error_sink = 
  let error_ploc = -99 in
  error_ploc, { nodes = NodeSet.empty ; succ = NodeMap.empty ; var = VariableMap.empty }

let rec get_next (lfrom, hfrom) stmts summary lto =
  let rec simplify_guard = let open Cfg in
    let var_eq a b =
      let get_node = function
        | Null  -> 0
        | Id id -> VariableMap.find id hfrom.var
        | Next id -> raise (Invalid_argument "unsupported comparison with next. introduce __dummy := x.next and compare __dummy instead.")
    in (get_node a) = (get_node b)
    in
    function
      | True -> true
      | False -> false
      | Neg g -> not(simplify_guard g)
      | Eq (a,b) -> var_eq a b
      | And (a,b) -> (simplify_guard a) && (simplify_guard b)
  in
  let project_onto s n =  NodeMap.filter (fun a _ -> NodeSet.mem a n) s in
  let reaches w n s =
    let rec closure boundary s =
      let new_boundary = NodeMap.fold (fun from to_ boundary -> if NodeSet.mem from boundary then NodeSet.add to_ boundary else boundary) s boundary in
      if new_boundary = boundary then boundary else (closure new_boundary s)
    in
    NodeSet.mem n (closure (NodeSet.singleton w) s)
  in
  let next_free_node nodes = let rec next_free_node_from i =
    if NodeSet.mem i nodes then next_free_node_from (i+1) else i in
    next_free_node_from 1
  in
  let merge s n m =
    let nodes = NodeSet.remove m s.nodes in
    let succ = NodeMap.add n (NodeMap.find m s.succ) (project_onto s.succ nodes) in
    { nodes ; succ ; var = s.var }
  in
  let split s n m =
    let nodes = NodeSet.add m s.nodes in
    let succ_n = NodeMap.find n s.succ in
    let succ = NodeMap.add m succ_n (NodeMap.add n m (NodeMap.remove n s.succ)) in
    { nodes ; succ ; var = s.var }
  in
  if equal (lfrom, hfrom) error_sink then
    [ [Assume True], error_sink ]
  else match stmts with
  | [ s ] -> ( match s with
    | Cfg.Alloc Cfg.Null                -> raise (Invalid_argument "null is not a valid lvalue")
    | Cfg.Alloc(Cfg.Next _)             -> raise (Invalid_argument "allocation of .next not implemented")
    | Cfg.Asgn (Cfg.Null, _)            -> raise (Invalid_argument "null is not a valid lvalue")
    | Cfg.Asgn (Cfg.Next _, Cfg.Next _) -> raise (Invalid_argument "assignment of .next := .next not implemented")
    | Cfg.Asgn (Cfg.Id u, Cfg.Null) -> begin
      let vu = VariableMap.find_opt u hfrom.var in
        match vu with
        | None ->
            let var = VariableMap.add u 0 hfrom.var in
            [ [Assume True], (lto, { nodes = hfrom.nodes; succ = hfrom.succ; var }) ]
        | Some 0 -> 
            (* A1 *)
            [ [Assume True], (lto, hfrom) ]
        | Some n ->
            (* prem 1,2 of A2 *)
            let exists_w_neq_u_st_w_neq_bot = VariableMap.exists (fun w vw -> w <> u && vw = n) hfrom.var in
            (* prem 2 of A2', A2'' *)
            let all_w_neq_u_st_Vofw_neq_n = VariableMap.for_all (fun w vw -> w = u || vw <> n) hfrom.var in
            (* prem 3 of A2' *)
            let n_has_two_preds = NodeSet.exists (fun p ->
              (NodeSet.exists (fun m ->
                p <> m &&
                (NodeMap.find p hfrom.succ) = n &&
                (NodeMap.find m hfrom.succ) = n)
              hfrom.nodes)
            ) hfrom.nodes in
            if
              exists_w_neq_u_st_w_neq_bot (* A2 *) ||
              all_w_neq_u_st_Vofw_neq_n && n_has_two_preds (* A2' *) then
              let var = VariableMap.add u 0 hfrom.var in
              [ [Assume True], (lto, { nodes = hfrom.nodes ; succ = hfrom.succ ; var }) ]
            else 
              (* prem 3,4 of A2'' *)
              let preds_m_of_n = NodeSet.filter (fun m -> (NodeMap.find m hfrom.succ) = n) hfrom.nodes in
              let n_has_single_pred_neq_n = (NodeSet.cardinal preds_m_of_n) = 1 && preds_m_of_n <> (NodeSet.singleton n) in
              if all_w_neq_u_st_Vofw_neq_n && n_has_single_pred_neq_n (* A2'' *) then
                let var = VariableMap.add u 0 hfrom.var in
                let m = List.hd (NodeSet.elements preds_m_of_n) in (* m is unique *)
                let ctr_m = ctr_of_node m in
                let ctr_n = ctr_of_node n in
                [ [Asgn (ctr_m, Add(ctr_m, Id ctr_n))], (lto, merge { nodes = hfrom.nodes ; succ = hfrom.succ ; var } m n) ]
            else
              (* prem 2 of A3, A3' *)
              let forall_w_neq_u_st_w_notreach_n = VariableMap.for_all (fun w vw -> w = u || not (reaches vw n hfrom.succ)) hfrom.var in
              (* prem 4 of A3' *)
              let succ_n = NodeMap.find n hfrom.succ in
              let exists_w_neq_u_st_Vofw_eq_m = VariableMap.exists (fun w vw -> w <> u && vw = succ_n) hfrom.var in
              (* prem 4 of A3'' *)
              let forall_w_neq_u_st_Vofw_neq_m = VariableMap.exists (fun w vw -> w = u || vw <> succ_n) hfrom.var in
              (* prem 5 of A3'' *)
              let succ_n_has_two_preds_neq_n = NodeSet.exists (fun p -> p <> n &&
                (NodeSet.exists (fun q -> q <> n && p <> q && (NodeMap.find p hfrom.succ) = succ_n && (NodeMap.find q hfrom.succ) = succ_n) hfrom.nodes)
              ) hfrom.nodes in
              if
              forall_w_neq_u_st_w_notreach_n && (succ_n = 0 || succ_n = n) (* A3 *) ||
              forall_w_neq_u_st_w_notreach_n && succ_n <> 0 && succ_n <> n && exists_w_neq_u_st_Vofw_eq_m (* A3' *) ||
              forall_w_neq_u_st_w_notreach_n && succ_n <> 0 && succ_n <> n && forall_w_neq_u_st_Vofw_neq_m && succ_n_has_two_preds_neq_n (* A3'' *) then
              let nodes = NodeSet.remove n hfrom.nodes in
              let succ = project_onto hfrom.succ nodes in
              let var = VariableMap.add u 0 hfrom.var in
              [ [Assume True], (lto, { nodes ; succ ; var }) ]
            else
              (* prem 5,6 of A3''' *)
              let preds_of_succ_n = NodeSet.filter (fun p -> p <> n && (NodeMap.find p hfrom.succ) = succ_n) hfrom.nodes in
              let succ_n_has_two_preds_neq_succ_n = (NodeSet.cardinal preds_of_succ_n) = 1 && (NodeSet.singleton succ_n) <> preds_of_succ_n in
              (* prem 5 of A3'''' *)
              let forall_p_neq_n_m_st_succ_p_neq_m = NodeSet.for_all (fun p -> p = n || p = succ_n || (NodeMap.find p hfrom.succ) <> succ_n) hfrom.nodes in
              if
              forall_w_neq_u_st_w_notreach_n && succ_n <> 0 && succ_n <> n && forall_w_neq_u_st_Vofw_neq_m && succ_n_has_two_preds_neq_succ_n (* A3''' *) then
              let p = List.hd (NodeSet.elements preds_of_succ_n) in
              let nodes = NodeSet.remove n hfrom.nodes in
              let succ = project_onto hfrom.succ nodes in
              let var = VariableMap.add u 0 hfrom.var in
              let ctr_p = ctr_of_node p in
              let ctr_m = ctr_of_node succ_n in
              [ [Asgn (ctr_p, Add(ctr_p, Id ctr_m))], (lto, merge { nodes ; succ ; var } p succ_n) ]
            else if
              forall_w_neq_u_st_w_notreach_n && succ_n <> 0 && succ_n <> n &&
              forall_w_neq_u_st_Vofw_neq_m && forall_p_neq_n_m_st_succ_p_neq_m (* A3'''' *) then
                let nodes = NodeSet.remove succ_n (NodeSet.remove n hfrom.nodes) in
                let succ = project_onto hfrom.succ nodes in
                let var = VariableMap.add u 0 hfrom.var in
                [ [Assume True], (lto, { nodes ; succ ; var }) ]
            else assert false
    end (* Cfg.Asgn (Cfg.Id u, Cfg.Null) *)
    | Cfg.Asgn (Cfg.Id u, Cfg.Id w) -> (* A4 *)
        let vu = VariableMap.find u hfrom.var in
        let vw = VariableMap.find w hfrom.var in
        let var = VariableMap.add u vw hfrom.var in
        assert (vu = 0) ;
        [ [Assume True], (lto, { nodes = hfrom.nodes ; succ = hfrom.succ ; var }) ]
    | Cfg.Alloc (Cfg.Id u) -> (* A5 *)
        let vu = VariableMap.find u hfrom.var in
        let n = next_free_node hfrom.nodes in
        let nodes = NodeSet.add n hfrom.nodes in
        let succ = NodeMap.add n 0 hfrom.succ in
        let var = VariableMap.add u n hfrom.var in
        assert (vu = 0) ;
        [ [Asgn (ctr_of_node n, Num 1)], (lto, { nodes ; succ ; var }) ]
    | Cfg.Asgn (Cfg.Id u, Cfg.Next w) -> (* A6, A7, A7' *)
        let vu = VariableMap.find u hfrom.var in
        let vw = VariableMap.find w hfrom.var in
        if (vw = 0) then (* A6 *)
          [ [ Assume True ], error_sink ]
        else (* A7, A7' *)
          let succ_n = NodeMap.find vw hfrom.succ in
          let m = next_free_node hfrom.nodes in
          let var1 = VariableMap.add u succ_n hfrom.var in
          let var2 = VariableMap.add u m hfrom.var in
          let ctr_n = ctr_of_node vw in
          let ctr_m = ctr_of_node m in
          assert (vu = 0) ;
          [ 
            [ Assume (Eq (ctr_n, 1)) ], (lto, { nodes = hfrom.nodes ; succ = hfrom.succ; var = var1 }) ; 
            [ Assume (Gt (ctr_n, 1)); Asgn (ctr_m, Add (ctr_n, Num (-1))); Asgn (ctr_n, Num 1) ], (lto, split { nodes = hfrom.nodes; succ = hfrom.succ; var = var2 } vw m)
          ]
    | Cfg.Asgn (Cfg.Next u, Cfg.Null) -> (* A8, A9 *)
        let n = VariableMap.find u hfrom.var in
        if (n = 0) then (* A8 *)
          [ [ Assume True ], error_sink ]
        else
          let succ_n = NodeMap.find n hfrom.succ in
          let ctr_n = ctr_of_node n in
          (* prem 4 of A9' *)
          let exists_w_neq_u_st_Vofw_eq_m = VariableMap.exists (fun w vw -> w <> u && vw = succ_n) hfrom.var in
          (* prem 4 of A9'', A9''' *)
          let forall_w_st_Vofw_neq_m = VariableMap.exists (fun w vw -> vw <> succ_n) hfrom.var in
          (* prem 5 of A3'' *)
          let succ_n_has_two_preds_neq_n = NodeSet.exists (fun p -> p <> n &&
            (NodeSet.exists (fun q -> q <> n && p <> q && (NodeMap.find p hfrom.succ) = succ_n && (NodeMap.find q hfrom.succ) = succ_n) hfrom.nodes)
            ) hfrom.nodes in
          if n <> 0 && (succ_n = 0 || succ_n = n) || (* A9 *)
            n <> 0 && succ_n <> 0 && succ_n <> n && exists_w_neq_u_st_Vofw_eq_m (* A9' *) ||
            n <> 0 && succ_n <> 0 && succ_n <> n && forall_w_st_Vofw_neq_m && succ_n_has_two_preds_neq_n (* A9'' *) then
            let succ = NodeMap.add n 0 hfrom.succ in
            [ [ Asgn (ctr_n, Num 1) ], (lto, { nodes = hfrom.nodes; succ; var = hfrom.var }) ]
          else
            (* prem 5,6 of A9''' *)
            let preds_of_succ_n = NodeSet.filter (fun p -> p <> n && (NodeMap.find p hfrom.succ) = succ_n) hfrom.nodes in
            let succ_n_has_two_preds_neq_succ_n = (NodeSet.cardinal preds_of_succ_n) = 1 && (NodeSet.singleton succ_n) <> preds_of_succ_n in
            if n <> 0 && succ_n <> 0 && succ_n <> n && forall_w_st_Vofw_neq_m && succ_n_has_two_preds_neq_succ_n (* A9''' *) then
            let p = List.hd (NodeSet.elements preds_of_succ_n) in
            let succ = NodeMap.add n 0 hfrom.succ in
            let ctr_p = ctr_of_node p in
            let ctr_n = ctr_of_node n in
            let ctr_m = ctr_of_node succ_n in
            (* A9''' *)
            [ [ Asgn (ctr_n, Num 1) ; Asgn (ctr_p, Add (ctr_p, Id ctr_m)) ], (lto, merge { nodes = hfrom.nodes ; succ ; var = hfrom.var } p succ_n) ]
            else 
            (* prem 4 of A3'''' *)
            let forall_p_neq_n_m_st_succ_p_neq_m = NodeSet.for_all (fun p -> p = n || p = succ_n || (NodeMap.find p hfrom.succ) <> succ_n) hfrom.nodes in
            let nodes = NodeSet.remove succ_n hfrom.nodes in
            let succ = NodeMap.add n 0 (project_onto hfrom.succ nodes) in
          
              if n <> 0 && succ_n <> 0 && succ_n <> n && forall_w_st_Vofw_neq_m && forall_p_neq_n_m_st_succ_p_neq_m then
                (* A9'''' *)
                [ [ Asgn (ctr_n, Num 1) ], (lto, {nodes ; succ ; var = hfrom.var }) ]
            else
              assert false
    | Cfg.Asgn (Cfg.Next u, Cfg.Id w) -> (* A10 *)
        let n = VariableMap.find u hfrom.var in
        let vw = VariableMap.find w hfrom.var in
        let succ_n = NodeMap.find n hfrom.succ in
        let succ = NodeMap.add n vw hfrom.succ in
        assert ((n <> 0) && (succ_n = 0)) ;
        [ [ Assume True ], (lto, { nodes = hfrom.nodes ; succ ; var = hfrom.var }) ]
    | Cfg.Assume g -> (match simplify_guard g with
      | true -> [ [ Assume True ], (lto, hfrom) ]
      | false -> [ [ Assume False ], (lto, hfrom) ]
    )
  )
  | stmts -> (* atomic sequence of statements *)
    Debugger.debug "ca_construction" "  ATOMIC TRANSITION COMPUTATION: %s\n" (Scfg.pprint_edge_kind summary);
    let cfg = Cfg.of_ast (Cfg.to_ast_stmt stmts) in
    let bicfg = of_cfg ~indent:2 ~introduce_assume_false:true cfg (0,hfrom) in
    (* Make sure the atomic CA is loop-free *)
    assert ((fst(G.scc bicfg)) = (G.nb_vertex bicfg)) ;
    let final_vertices = G.fold_vertex (fun v l -> match G.succ bicfg v with [] -> v :: l | _ -> l) bicfg [] in
    let paths_final_v = G.all_paths bicfg (0, hfrom) final_vertices in
    let paths = List.map (fun (path, (ploc, heap)) ->
      let var = VariableMap.filter (fun v n -> not (Str.string_match (Str.regexp "^__") v 0)) heap.var in
      let heap = { nodes = heap.nodes ; succ = heap.succ ; var } in
      path, (ploc, heap)
    ) paths_final_v in
    let paths_seq = List.map (fun (path, (ploc, heap)) ->
      List.fold_right (fun (_, (stmts,_), _) seq -> stmts @ seq) path [],
      if equal (ploc,heap) error_sink then (ploc, heap) else (lto, heap)
    ) paths in
    paths_seq
and of_cfg ?(indent=0) ?(introduce_assume_false=false) cfg init_ca_loc =
  (* Rationale for `introduce_assume_false':
   * For ordinary CA construction, it makes no sense to follow atomic statements
   * that include an `assume(false)' and `introduce_assume_false' should be kept
   * at `false'.
   * However, if we construct a CA for an atomic statement (in order to later
   * read statements off its paths), we need to keep the `assume(false)'
   * *in* the CA to retain semantic equivalence. We still stop exploring the
   * path *after* the `assume(false)'.
   *)
  let g = G.Imp.create () in
  let q = Queue.create () in
  let indent = String.make indent ' ' in
  let find_isomorphic_heap g (to_ploc, heap) =
    let count_nodes_mapped_to_0 node_map =
      NodeMap.fold (fun var node num ->
        match node with 0 -> (num + 1) | _ -> num
      ) node_map 0
    in
    let count_vars_mapped_to_0 var_map =
      VariableMap.fold (fun var node num ->
        match node with 0 -> (num + 1) | _ -> num
      ) var_map 0
    in
    let permutation =
      if (NodeSet.cardinal heap.nodes) < 8 then (
        let permutations = Util.permutations (NodeSet.elements heap.nodes) in
        let cand_vertices = G.Imp.fold_vertex (fun (cand_ploc, cand_heap) l ->
          if cand_ploc = to_ploc &&
             (NodeSet.cardinal cand_heap.nodes) = (NodeSet.cardinal heap.nodes) &&
             (count_nodes_mapped_to_0 cand_heap.succ) = (count_nodes_mapped_to_0 heap.succ) &&
             (VariableMap.cardinal cand_heap.var) = (VariableMap.cardinal heap.var) &&
             (count_vars_mapped_to_0 cand_heap.var) = (count_vars_mapped_to_0 heap.var)
          then
            (cand_ploc, cand_heap) :: l
          else
            l
        ) g []
        in
        Util.List.fold_until_some (fun permutation ->
          let permuted_heap = Ca_vertex.permute heap permutation in
          match List.find_opt (Ca_vertex.equal (to_ploc, permuted_heap)) cand_vertices with
          | Some _ -> Some permutation
          | _ -> None
        ) permutations
      )
      else
        None
    in
    permutation
  in
  let rec rename_max_node_fp g tstmt to_vertex =
    let rename_max_node g (_, heap) =
      let max_elt = NodeSet.max_elt_opt heap.nodes in
      match max_elt with 
      | Some max_elt ->
        let (--) i j = let rec aux n acc = if n < i then acc else aux (n-1) (n :: acc) in aux j [] in
        let enumerated_max = List.fold_left (fun set elt -> NodeSet.add elt set) NodeSet.empty (1--max_elt) in
        let novel_nodes = NodeSet.diff enumerated_max heap.nodes in
        let rename_structure h from to_ =
          let rename_node node = if node = from then to_ else node in
          let nodes = NodeSet.map rename_node h.nodes in
          let succ = List.fold_left (fun map (f,t) -> NodeMap.add (rename_node f) (rename_node t) map) NodeMap.empty (NodeMap.bindings h.succ) in
          let var = List.fold_left (fun map (f,t) -> VariableMap.add f (rename_node t) map) VariableMap.empty (VariableMap.bindings h.var) in
          { nodes ; succ ; var }
        in
        if (NodeSet.cardinal novel_nodes) > 0 then
          let novel_node = NodeSet.min_elt novel_nodes in
          Some (max_elt, novel_node, rename_structure heap max_elt novel_node)
        else
          None
      | None -> None
    in
    match rename_max_node g to_vertex with
    | Some (rename_from, rename_to, renamed_structure) ->
        let tstmt = tstmt @ [ Asgn (ctr_of_node rename_to, Id (ctr_of_node rename_from)) ] in
        Debugger.debug_nocomp "ca_construction" " -rename-> %s (%s)" (pprint (rename_to, renamed_structure)) (pprint_seq ~sep:"; " tstmt) ;
        rename_max_node_fp g tstmt (fst to_vertex, renamed_structure)
    | None ->
        tstmt, to_vertex
  in
  (* add inital vertex to graph and worklist *)
  G.Imp.add_vertex g init_ca_loc ;
  Queue.add init_ca_loc q ;
  (* while there are nodes in the worklist *)
  while not (Queue.is_empty q) do
    let from_vertex = Queue.pop q in
    let from, from_heap = from_vertex in
    Cfg.G.iter_succ_e (fun (from, (stmt, summary), to_) -> begin
      Debugger.debug "ca_construction" "%s%d -> %d (%s) => %s ->\n" indent from to_ (Cfg.pprint_seq ~sep:"; " stmt) (pprint from_vertex) ;
      let translated = get_next from_vertex stmt summary to_ in
      List.iter (fun (tstmt, to_vertex) ->
        let has_assume_false = List.mem (Assume False) tstmt in
        if has_assume_false && not introduce_assume_false then ()
        else begin
          let tstmt = List.filter (fun stmt -> stmt <> Assume True) tstmt in
          let tstmt = match tstmt with [] -> [ Assume True ] | _ -> tstmt in
          Debugger.debug "ca_construction" "%s%s%s (%s)" indent indent (pprint to_vertex) (pprint_seq ~sep:"; " tstmt) ;
          let tstmt, to_vertex = rename_max_node_fp g tstmt to_vertex in
          let tstmt, to_vertex =
            if (not !Config.iso) || introduce_assume_false || G.Imp.mem_vertex g to_vertex then
              tstmt, to_vertex
            else
              match find_isomorphic_heap g to_vertex with
              | Some permutation ->
                  let _, to_heap = to_vertex in
                  let permuted_heap = Ca_vertex.permute to_heap permutation in
                  let tstmt = tstmt @
                    (List.map (fun node -> Asgn (tmp_ctr_of_node node, Id (ctr_of_node node))) (NodeSet.elements permuted_heap.nodes)) @
                    (List.map (fun node -> Asgn (ctr_of_node node, Id (tmp_ctr_of_node ((Util.List.find node permutation)+1)))) (NodeSet.elements permuted_heap.nodes))
                  in
                  Debugger.debug_nocomp "ca_construction" " -isomorph-> <%s> %s (%s)" (Util.List.pprint string_of_int permutation) (pprint (to_, permuted_heap)) (pprint_seq ~sep:"; " tstmt);
                  tstmt, (to_, permuted_heap)
              | None ->
                  tstmt, to_vertex
          in
          let has_to_vertex = G.Imp.mem_vertex g to_vertex in
          Debugger.debug_nocomp "ca_construction" "\n" ;
          G.Imp.add_edge_e g (from_vertex, (tstmt, summary), to_vertex) ;
          if not has_assume_false && not has_to_vertex && not (equal to_vertex error_sink) then Queue.add to_vertex q
        end
      ) translated
      end
    ) cfg from
  done ;
  G.Imp.fold_edges_e (fun edge acc_g -> G.add_edge_e acc_g edge) g G.empty

let structure_from_parsed_heaps heaps =
  List.map (fun heap -> 
    let nodes = List.fold_left (fun set (id, _, _, _) -> if id > 0 then NodeSet.add id set else set) NodeSet.empty heap in
    let succ = List.fold_left (fun map (id, next, _, _) -> match next with
      | Some n -> NodeMap.add id n map | None -> map
    ) NodeMap.empty heap in
    let var = List.fold_left (fun map (id, _, pvars, _) -> 
      List.fold_left (fun m pvar -> VariableMap.add pvar id m) map pvars
    ) VariableMap.empty heap in
    let constr = List.fold_left (fun map (id,_,_,constr) -> match constr with
      | Some upper_bound ->
          let interval = Apron.Interval.of_int 1 upper_bound in
          VariableMap.add (ctr_of_node id) interval map
      | None -> map
    ) VariableMap.empty heap in
    (0, { nodes ; succ ; var }), constr
  ) heaps

let collect_vars cfg =
  let (--) i j = let rec aux n acc = if n < i then acc else aux (n-1) (n :: acc) in aux j [] in
  let max_node = G.fold_vertex (fun (ploc, heap) m ->
    match NodeSet.max_elt_opt heap.nodes with
      | Some n -> max n m
      | None -> m
  ) cfg 0 in
  let vars_vertex = List.map ctr_of_node (1--max_node) in
  let collect_vars_seq =
    let rec collect_vars_nexpr = function
      | Id id       -> [id]
      | Num _       -> []
      | Add (id, e) -> id :: (collect_vars_nexpr e)
    in
    let rec collect_vars_bexpr = function
      | True | False            -> []
      | Eq (id, _) | Gt (id, _) -> [id]
      | Neg e                   -> collect_vars_bexpr e
    in
    let collect_vars_stmt = function
      | Assume e     -> collect_vars_bexpr e
      | Asgn (id, e) -> id :: (collect_vars_nexpr e)
    in
    List.fold_left (fun l stmt -> (collect_vars_stmt stmt) @ l) []
  in
  let vars_edge = G.fold_edges_e (fun (_, (stmts, _), _) l -> (collect_vars_seq stmts) @ l) cfg [] in
  let id_list = List.sort_uniq Pervasives.compare (vars_vertex @ vars_edge) in
  id_list


let propagate_equalities ca =
  let ca' = G.imp_of_perv ca in
  let q = Queue.create () in
  G.Imp.iter_vertex (fun v -> Queue.add v q) ca' ;
  while not (Queue.is_empty q) do
    let v = Queue.pop q in
    let pred_stmt = List.map (fun (_,(stmt,_),_) -> stmt) (G.Imp.pred_e ca' v) in
    let stmt_to_propagate = match pred_stmt with
    | stmt :: tl -> ( match stmt with
      | [ Asgn (id, Id id2) ] -> if List.for_all ((=) stmt) tl then Some stmt else None
      | _ -> None
    )
    | [] -> None
    in
    match stmt_to_propagate with
    | Some [ stmt_prop ] ->
        Debugger.debug "propagate_eq" "%s %s\n" (Ca_vertex.pprint v) (pprint_stmt stmt_prop) ;
        G.Imp.iter_succ_e (fun edge ->
          let (_,(stmt,ek),v') = edge in
          if (List.hd stmt) <> stmt_prop then begin
            let stmt' = match stmt with
              | [ Assume True ] -> [ stmt_prop ]
              | _ -> stmt_prop :: stmt
            in
            let edge' = v,(stmt',ek),v' in
            G.Imp.remove_edge_e ca' edge ;
            G.Imp.add_edge_e ca' edge' ;
            Debugger.debug "propagate_eq" "  %s\n    %s\n" (G.pprint_edge edge') (pprint_seq ~sep:"; " stmt') ;
            Queue.add v' q
          end
        ) ca' v
    | _ -> ()
  done ;
  G.of_imp ca'
