open Graph

open Ast

type identifier = string

type vertex = int * int * identifier (* id, next, pvar *)

type heap = vertex list

module Int = struct
  let compare = Pervasives.compare
  type t = int
end
module NS = Set.Make(Int)
module NodeSet = struct
  include NS
  let to_string s = "{" ^ (String.concat "," (List.map string_of_int (NS.elements s))) ^ "}"
end
module SM = Map.Make(Int)
module SuccMap = struct
  include SM
  let to_string m = "{" ^ (String.concat "," (List.map (fun (from,to_) -> Printf.sprintf "%d:%d" from to_) (SM.bindings m))) ^ "}"
end
module VM = Map.Make(String)
module VarMap = struct
  include VM
  let to_string m = "{" ^ (String.concat "," (List.map (fun (var,node) -> Printf.sprintf "%s:%d" var node) (VM.bindings m))) ^ "}"
end

type structure = { nodes : NodeSet.t ; succ : int SuccMap.t ; var : int VarMap.t }

let structure_equal a b = 
  let nodes_equal = NodeSet.equal a.nodes b.nodes in
  let succ_equal = SuccMap.equal (=) a.succ b.succ in
  let var_equal = VarMap.equal (=) a.var b.var in
  nodes_equal && succ_equal && var_equal

let structure_from_heap heap =
  let nodes = List.fold_left (fun set (id, next, pvar) -> NodeSet.add id set) NodeSet.empty heap in
  let succ = List.fold_left (fun map (id, next, pvar) -> SuccMap.add id next map) SuccMap.empty heap in
  let var = List.fold_left (fun map (id, next, pvar) -> VarMap.add pvar id map) VarMap.empty heap in
  { nodes ; succ ; var }

let dump_str ?(sep=", ") s =
  String.concat sep [ "nodes: " ^ NodeSet.to_string s.nodes ; "succs: " ^ SuccMap.to_string s.succ ; "vars: " ^ VarMap.to_string s.var ]

let project_onto s n = (* project s onto n *)
  SuccMap.filter (fun a _ -> NodeSet.mem a n) s

let merge s n m =
  let nodes = NodeSet.remove m s.nodes in
  let succ = SuccMap.add n (SuccMap.find m s.succ) (project_onto s.succ nodes) in
  { nodes ; succ ; var = s.var }

let split s n m =
  let nodes = NodeSet.add m s.nodes in
  let succ_n = SuccMap.find n s.succ in
  let succ = SuccMap.add m succ_n (SuccMap.add n m (SuccMap.remove n s.succ)) in
  { nodes ; succ ; var = s.var }

let rec closure boundary s =
  let new_boundary = SuccMap.fold (fun from to_ boundary -> if NodeSet.mem from boundary then NodeSet.add to_ boundary else boundary) s boundary in
  if new_boundary = boundary then boundary else (closure new_boundary s)

let reaches w n s =
  NodeSet.mem n (closure (NodeSet.singleton w) s)

let ctr_of_node n = Printf.sprintf "x_%d" n

type ploc = int
type cloc = ploc * structure

let dump_cloc ?(sep=", ") (p, s) = Printf.sprintf "pc: %d%s%s" p sep (dump_str ~sep s)
let cloc_equal (a_ploc, a_heap) (b_ploc, b_heap) =
    a_ploc = b_ploc && (structure_equal a_heap b_heap)

module V_ = struct
  type t = cloc
  let compare = fun x y -> compare (dump_cloc x) (dump_cloc y)
  let hash = fun x -> Hashtbl.hash (dump_cloc x)
  let equal = cloc_equal
end

type cfg_edge = Cfg.V_.t * Cfg.E_.t * Cfg.V_.t

module E_ = struct
  type t = Ast.stmt * cfg_edge
  let compare = compare
  let default = Assume True, (0, (Assume True, 0), 0)
end

module G = Imperative.Digraph.ConcreteBidirectionalLabeled(V_)(E_)

module Dot_ = Graphviz.Dot (struct
  include G
  let vertex_name (p,s) = string_of_int (Hashtbl.hash ((string_of_int p) ^ "__" ^ (dump_str s)))
  let graph_attributes _ = []
  let default_vertex_attributes _ = [`Shape `Box; `Regular false]
  let vertex_attributes c = [`Label (dump_cloc ~sep:"\n" c)]
  let default_edge_attributes _ = []
  let edge_attributes (v1, (stmt, (_, (_, summary), _)), v2) = [
    `Label (Ast.pprint ~atomic_angles:false stmt) ;
    `Color (Colormap.get_color summary) ;
    `Fontcolor (Colormap.get_color summary)
  ]
  let get_subgraph _ = None
end)
module Dot = struct
  include Dot_
  let write_dot g path = let chout = open_out path in Dot_.output_graph chout g ; close_out chout
end

module W_ = struct
  type edge = G.E.t
  type t = int
  let weight _ = 1
  let compare = compare
  let add a b = a+b
  let zero = 0
end

module Dijkstra = Path.Dijkstra(G)(W_)
module SCC = Components.Make(G)

let next_free_node nodes =
  let rec x_next_free_node i =
    if NodeSet.mem i nodes then x_next_free_node (i+1) else i
  in
  x_next_free_node 1

let reduce_seq_stmts seq final_vars =
  let rec x_reduce_seq_stmts seq final_vars =
    let rec def var seq = match seq with
    | Asgn (v, expr) :: tl when v = var -> true
    | hd :: tl -> def var tl
    | [] -> false
    in
    let rec use_expr var e = match e with
    | Id id -> var = id
    | Num _ -> false
    | Add (id, expr) -> var = id || use_expr var expr
    in
    let rec use_guard var g = match g with
    | Eq (id, e) -> id = var || use_expr var e
    | Gt (id, e) -> id = var || use_expr var e
    | Neg g      -> use_guard var g
    | True       -> false
    | False      -> false
    | _ -> assert false
    in
    let rec use var seq = match seq with
      | s :: tl -> begin
        match s with
        | Asgn (id,e) -> use_expr var e
        | Assume g    -> use_guard var g
        | Atomic l    -> use var l
        | _ -> assert false
      end || use var tl
      | [] -> false
    in
    (* Filter assume(true); *)
    let filter_assume_true seq = List.filter (fun stmt -> match stmt with Assume True -> false | _ -> true) seq in
    let rec def_use seq = 
      let final_ctrs = List.map ctr_of_node (NodeSet.elements final_vars) in
      let rec replace_seq seq v replacement = 
        let rec replace_expr e v replacement = match e with
        | Id id          -> if id = v then replacement else Id id
        | Add (id, expr) -> begin
            if id <> v then
              Add (id, replace_expr expr v replacement)
            else match expr with
              | Id id2 -> Add (id2, Id id)
              | expr -> assert false
        end
        | Num num -> Num num
        in
        (* let rec replace_guard g v replacement = match g with *)
        (* | Eq (id, expr) -> assert (id <> v); Eq (id, (replace_expr expr v replacement)) *)
        (* | Gt (id, expr) -> assert (id <> v); Gt (id, (replace_expr expr v replacement)) *)
        (* | EqNull id -> assert (id <> v); EqNull id *)
        (* | Neg g -> Neg (replace_guard g v replacement) *)
        (* in *)
        match seq with
        | Asgn (id, expr) :: tl -> Asgn (id, replace_expr expr v replacement) :: (replace_seq tl v replacement)
        (* | Assume g :: tl -> Assume (replace_guard g v replacement) :: (replace_seq tl v replacement) *)
        | hd :: tl -> raise (Invalid_argument (Printf.sprintf "Replacement on non-assignment: %s" (Ast.pprint hd)))
        | [] -> []
      in
      let rec vars_in_expr e = match e with
        | Add (id, expr) -> id :: vars_in_expr expr
        | Id id -> [id]
        | Num num -> []
      in
      match seq with
      | Asgn (v, expr) :: tl ->
          let remove_last l = match (List.rev l) with | h::t -> List.rev t | [] -> [] in
          let vars_in_expr_ = vars_in_expr expr in
          let vars_in_expr_def_in_tl_but_last = List.fold_left (fun b v -> b || (def v (remove_last tl))) false vars_in_expr_ in
          if def v tl && not (use v tl) then
            (* Filter defs if there is a later def without any use *)
            def_use tl
          else if not (List.mem v final_ctrs) && not vars_in_expr_def_in_tl_but_last then
            (* Filter defs if the assigned counter refers to a node no longer present *)
            def_use (replace_seq tl v expr)
          else
            Asgn (v, expr) :: (def_use tl)
      | hd :: tl -> hd :: (def_use tl)
      | [] -> []
    in
    def_use (def_use (filter_assume_true seq))
  in
  (* Printf.printf "REDUCING %s\n" (Ast.pprint_seq ~sep:"; " seq) ; *)
  let fixpoint =  x_reduce_seq_stmts seq final_vars in
  match List.length fixpoint with
    | 0 -> Assume True
    | 1 -> List.hd fixpoint
    | _ -> Atomic fixpoint

let find_isomorphic_heap g stmt to_vertex =
  let to_ploc, heap = to_vertex in
  let max_elt = NodeSet.max_elt_opt heap.nodes in
  match max_elt with Some max_elt ->
    let (--) i j = let rec aux n acc = if n < i then acc else aux (n-1) (n :: acc) in aux j [] in
    let enumerated_max = List.fold_left (fun set elt -> NodeSet.add elt set) NodeSet.empty (1--max_elt) in
    let novel_nodes = NodeSet.diff enumerated_max heap.nodes in
    let rename_structure h from to_ =
      let rename_node node = if node = from then to_ else node in
      let nodes = NodeSet.map rename_node h.nodes in
      let succ = List.fold_left (fun map (f,t) -> SuccMap.add (rename_node f) (rename_node t) map) SuccMap.empty (SuccMap.bindings h.succ) in
      let var = List.fold_left (fun map (f,t) -> VarMap.add f (rename_node t) map) VarMap.empty (VarMap.bindings h.var) in
      { nodes ; succ ; var }
    in
    let isomorphic_structures = List.map (fun novel_node ->
      let rewrites = List.map (fun potentially_discarded_node -> potentially_discarded_node, novel_node) (NodeSet.elements heap.nodes) in
      let isomporphic_structure_candidates = List.map (fun (f,t) -> f, t, rename_structure heap f t) rewrites in
      let isomorphic_structures = List.filter (fun (f, t, structure) -> G.mem_vertex g (to_ploc, structure)) isomporphic_structure_candidates in
      isomorphic_structures
    ) (NodeSet.elements novel_nodes) in
    let isomorphic_structures = List.concat isomorphic_structures in
    if (List.length isomorphic_structures) > 0 then
      (* isomorphic structures found in the graph *)
      isomorphic_structures, true
    else if (NodeSet.cardinal novel_nodes) > 0 then
      (* no isomorphic structures found; still rename the max node *)
      let novel_node = NodeSet.min_elt novel_nodes in
      [ max_elt, novel_node, rename_structure heap max_elt novel_node ], false
    else
      (* no isomorphic structures found; no renaming possible (|nodes| = max {nodes}) *)
      [], false
    | None -> [], false

let rec convert ?(indent=0) ?(prune_infeasible=true) init_heap cfg =
  let g = G.create () in
  let q = Queue.create () in
  let indent = String.make indent ' ' in
    G.add_vertex g (0, init_heap) ;
    Queue.add (0, init_heap) q ;
    while not (Queue.is_empty q) do
      let from_vertex = Queue.pop q in
      let from, from_heap = from_vertex in
      Cfg.G.iter_succ_e (fun (from, (stmt, summary), to_) -> begin
        Debugger.logf Debugger.Info "%s%d -> %d (%s) => %s -> " indent from to_ (Ast.pprint ~sep:"; " stmt) (dump_cloc from_vertex) ;
        let translated = l2ca from_heap stmt in
        List.iter (fun (tstmt, to_heap) ->
            let to_vertex = to_, to_heap in
            let isomorphic_structures, found_isomorphic = find_isomorphic_heap g tstmt to_vertex in
            let isomorphic_structures_num = List.length isomorphic_structures in
            let tstmt, to_heap =
              try
                match isomorphic_structures with
                | isomorphic_structure :: tl ->
                    let f, t, structure = isomorphic_structure in
                    let rewritten_stmt = reduce_seq_stmts [ tstmt ; Asgn (ctr_of_node t, Id (ctr_of_node f)) ] structure.nodes in
                    rewritten_stmt, structure
                | _ -> tstmt, to_heap
              with
                | Invalid_argument msg -> tstmt, to_heap
            in
            (* prune infeasible assume edges *)
            let tstmt = match tstmt with
            | Assume Eq (id1, Id id2)     -> Assume (ttolit ((VarMap.find id1 from_heap.var) = (VarMap.find id2 from_heap.var)))
            | Assume Neg Eq (id1, Id id2) -> Assume (ttolit ((VarMap.find id1 from_heap.var) <> (VarMap.find id2 from_heap.var)))
            | Assume EqNull id            -> Assume (ttolit ((VarMap.find id from_heap.var) = 0))
            | Assume Neg EqNull id        -> Assume (ttolit ((VarMap.find id from_heap.var) <> 0))
            | _ -> tstmt
            in
            Debugger.logf Debugger.Info "%s (%s)" (dump_cloc to_vertex) (Ast.pprint ~sep:"; " tstmt) ;
            let to_vertex = to_, to_heap in
            if tstmt = Assume False then
              begin
                Debugger.logf Debugger.Info "\n" ;
                match prune_infeasible with
                | true -> ()
                | false -> G.add_edge_e g (from_vertex, (tstmt, (from, (stmt, summary), to_)), to_vertex) ;
              end
            else begin
              let has_to_vertex = G.mem_vertex g to_vertex in
              if isomorphic_structures_num > 0 then
                if found_isomorphic then
                  Debugger.logf Debugger.Info " ~ %s (%s) [out of %d isomorphic structures]\n" (dump_cloc to_vertex) (Ast.pprint ~sep:"; " tstmt) isomorphic_structures_num
                else
                  Debugger.logf Debugger.Info " -r-> %s (%s) [0 isomorphic structures found; renaming node]\n" (dump_cloc to_vertex) (Ast.pprint ~sep:"; " tstmt)
              else
                Debugger.logf Debugger.Info " [0 isomorphic structures found]\n"
              ;
              G.add_edge_e g (from_vertex, (tstmt, (from, (stmt, summary), to_)), to_vertex) ;
              if not has_to_vertex then Queue.add to_vertex q
            end
        ) translated
        end
      ) cfg from
    done
    ;
    g
and l2ca hfrom stmt =
  match stmt with
  | AsgnNull u -> begin
    let vu = VarMap.find_opt u hfrom.var in
      match vu with
      | None ->
          let var = VarMap.add u 0 hfrom.var in
          [ Assume True, { nodes = hfrom.nodes; succ = hfrom.succ; var } ]
      | Some 0 -> 
          (* A1 *)
          [ Assume True, hfrom ]
      | Some n ->
          (* prem 1,2 of A2 *)
          let exists_w_neq_u_st_w_neq_bot = VarMap.exists (fun w vw -> w <> u && vw = n) hfrom.var in
          (* prem 2 of A2', A2'' *)
          let all_w_neq_u_st_Vofw_neq_n = VarMap.for_all (fun w vw -> w = u || vw <> n) hfrom.var in
          (* prem 3 of A2' *)
          let n_has_two_preds = NodeSet.exists (fun p ->
            (NodeSet.exists (fun m ->
              p <> m &&
              (SuccMap.find p hfrom.succ) = n &&
              (SuccMap.find m hfrom.succ) = n)
            hfrom.nodes)
          ) hfrom.nodes in
          if
            exists_w_neq_u_st_w_neq_bot (* A2 *) ||
            all_w_neq_u_st_Vofw_neq_n && n_has_two_preds (* A2' *) then
            let var = VarMap.add u 0 hfrom.var in
            [ Assume True, { nodes = hfrom.nodes ; succ = hfrom.succ ; var } ]
          else 
            (* prem 3,4 of A2'' *)
            let preds_m_of_n = NodeSet.filter (fun m -> (SuccMap.find m hfrom.succ) = n) hfrom.nodes in
            let n_has_single_pred_neq_n = (NodeSet.cardinal preds_m_of_n) = 1 && preds_m_of_n <> (NodeSet.singleton n) in
            if all_w_neq_u_st_Vofw_neq_n && n_has_single_pred_neq_n (* A2'' *) then
              let var = VarMap.add u 0 hfrom.var in
              let m = List.hd (NodeSet.elements preds_m_of_n) in (* m is unique *)
              let ctr_m = ctr_of_node m in
              let ctr_n = ctr_of_node n in
              [ Asgn (ctr_m, Add(ctr_m, Id ctr_n)), merge { nodes = hfrom.nodes ; succ = hfrom.succ ; var } m n ]
          else
            (* prem 2 of A3, A3' *)
            let forall_w_neq_u_st_w_notreach_n = VarMap.for_all (fun w vw -> w = u || not (reaches vw n hfrom.succ)) hfrom.var in
            (* prem 4 of A3' *)
            let succ_n = SuccMap.find n hfrom.succ in
            let exists_w_neq_u_st_Vofw_eq_m = VarMap.exists (fun w vw -> w <> u && vw = succ_n) hfrom.var in
            (* prem 4 of A3'' *)
            let forall_w_neq_u_st_Vofw_neq_m = VarMap.exists (fun w vw -> w = u || vw <> succ_n) hfrom.var in
            (* prem 5 of A3'' *)
            let succ_n_has_two_preds_neq_n = NodeSet.exists (fun p -> p <> n &&
              (NodeSet.exists (fun q -> q <> n && p <> q && (SuccMap.find p hfrom.succ) = succ_n && (SuccMap.find q hfrom.succ) = succ_n) hfrom.nodes)
            ) hfrom.nodes in
            if
            forall_w_neq_u_st_w_notreach_n && (succ_n = 0 || succ_n = n) (* A3 *) ||
            forall_w_neq_u_st_w_notreach_n && succ_n <> 0 && succ_n <> n && exists_w_neq_u_st_Vofw_eq_m (* A3' *) ||
            forall_w_neq_u_st_w_notreach_n && succ_n <> 0 && succ_n <> n && forall_w_neq_u_st_Vofw_neq_m && succ_n_has_two_preds_neq_n (* A3'' *) then
            let nodes = NodeSet.remove n hfrom.nodes in
            let succ = project_onto hfrom.succ nodes in
            let var = VarMap.add u 0 hfrom.var in
            [ Assume True, { nodes ; succ ; var } ]
          else
            (* prem 5,6 of A3''' *)
            let preds_of_succ_n = NodeSet.filter (fun p -> p <> n && (SuccMap.find p hfrom.succ) = succ_n) hfrom.nodes in
            let succ_n_has_two_preds_neq_succ_n = (NodeSet.cardinal preds_of_succ_n) = 1 && (NodeSet.singleton succ_n) <> preds_of_succ_n in
            (* prem 5 of A3'''' *)
            let forall_p_neq_n_m_st_succ_p_neq_m = NodeSet.for_all (fun p -> p = n || p = succ_n || (SuccMap.find p hfrom.succ) <> succ_n) hfrom.nodes in
            if
            forall_w_neq_u_st_w_notreach_n && succ_n <> 0 && succ_n <> n && forall_w_neq_u_st_Vofw_neq_m && succ_n_has_two_preds_neq_succ_n (* A3''' *) then
            let p = List.hd (NodeSet.elements preds_of_succ_n) in
            let nodes = NodeSet.remove n hfrom.nodes in
            let succ = project_onto hfrom.succ nodes in
            let var = VarMap.add u 0 hfrom.var in
            let ctr_p = ctr_of_node p in
            let ctr_m = ctr_of_node succ_n in

            [ Asgn (ctr_p, Add(ctr_p, Id ctr_m)), merge { nodes ; succ ; var } p succ_n ]
          else if
            forall_w_neq_u_st_w_notreach_n && succ_n <> 0 && succ_n <> n &&
            forall_w_neq_u_st_Vofw_neq_m && forall_p_neq_n_m_st_succ_p_neq_m (* A3'''' *) then
              let nodes = NodeSet.remove succ_n (NodeSet.remove n hfrom.nodes) in
              let succ = project_onto hfrom.succ nodes in
              let var = VarMap.add u 0 hfrom.var in
              [ Assume True, { nodes ; succ ; var } ]
          else assert false
  end
  | Asgn (u, w) -> (* A4 *)
      (match w with | Id w ->
        let vu = VarMap.find u hfrom.var in
        let vw = VarMap.find w hfrom.var in
        let var = VarMap.add u vw hfrom.var in
        assert (vu = 0) ;
        [ Assume True, { nodes = hfrom.nodes ; succ = hfrom.succ ; var } ]
      | _ -> assert false)
  | Alloc u -> (* A5 *)
      let vu = VarMap.find u hfrom.var in
      let n = next_free_node hfrom.nodes in
      let nodes = NodeSet.add n hfrom.nodes in
      let succ = SuccMap.add n 0 hfrom.succ in
      let var = VarMap.add u n hfrom.var in
      assert (vu = 0) ;
      [ Asgn (ctr_of_node n, Num 1), { nodes ; succ ; var } ]
  | AsgnNext (u, w) -> (* A7, A7' *)
      let vu = VarMap.find u hfrom.var in
      let vw = VarMap.find w hfrom.var in
      let succ_n = SuccMap.find vw hfrom.succ in
      let m = next_free_node hfrom.nodes in
      let var1 = VarMap.add u succ_n hfrom.var in
      let var2 = VarMap.add u m hfrom.var in
      let ctr_n = ctr_of_node vw in
      let ctr_m = ctr_of_node m in
      assert (vu = 0) ;
      assert (vw <> 0) ; (* A6 *)
        [ Assume (Eq (ctr_n, Num 1)), { nodes = hfrom.nodes ; succ = hfrom.succ; var = var1 } ; 
          Atomic [Assume (Gt (ctr_n, Num 1)); Asgn (ctr_m, Add (ctr_n, Num (-1))); Asgn (ctr_n, Num 1)], split { nodes = hfrom.nodes; succ = hfrom.succ; var = var2 } vw m
        ]
  | NextAsgnNull u ->
      let n = VarMap.find u hfrom.var in
      let succ_n = SuccMap.find n hfrom.succ in
      let ctr_n = ctr_of_node n in
      (* prem 4 of A9' *)
      let exists_w_neq_u_st_Vofw_eq_m = VarMap.exists (fun w vw -> w <> u && vw = succ_n) hfrom.var in
      (* prem 4 of A9'', A9''' *)
      let forall_w_st_Vofw_neq_m = VarMap.exists (fun w vw -> vw <> succ_n) hfrom.var in
      (* prem 5 of A3'' *)
      let succ_n_has_two_preds_neq_n = NodeSet.exists (fun p -> p <> n &&
        (NodeSet.exists (fun q -> q <> n && p <> q && (SuccMap.find p hfrom.succ) = succ_n && (SuccMap.find q hfrom.succ) = succ_n) hfrom.nodes)
        ) hfrom.nodes in
      if n <> 0 && (succ_n = 0 || succ_n = n) || (* A9 *)
         n <> 0 && succ_n <> 0 && succ_n <> n && exists_w_neq_u_st_Vofw_eq_m (* A9' *) ||
         n <> 0 && succ_n <> 0 && succ_n <> n && forall_w_st_Vofw_neq_m && succ_n_has_two_preds_neq_n (* A9'' *) then
        let succ = SuccMap.add n 0 hfrom.succ in
        [ Asgn (ctr_n, Num 1), { nodes = hfrom.nodes; succ; var = hfrom.var } ]
      else
        (* prem 5,6 of A9''' *)
        let preds_of_succ_n = NodeSet.filter (fun p -> p <> n && (SuccMap.find p hfrom.succ) = succ_n) hfrom.nodes in
        let succ_n_has_two_preds_neq_succ_n = (NodeSet.cardinal preds_of_succ_n) = 1 && (NodeSet.singleton succ_n) <> preds_of_succ_n in
        if n <> 0 && succ_n <> 0 && succ_n <> n && forall_w_st_Vofw_neq_m && succ_n_has_two_preds_neq_succ_n (* A9''' *) then
        let p = List.hd (NodeSet.elements preds_of_succ_n) in
        let succ = SuccMap.add n 0 hfrom.succ in
        let ctr_p = ctr_of_node p in
        let ctr_n = ctr_of_node n in
        let ctr_m = ctr_of_node succ_n in
        (* A9''' *)
        [ Atomic [ Asgn (ctr_n, Num 1) ; Asgn (ctr_p, Add (ctr_p, Id ctr_m)) ], merge { nodes = hfrom.nodes ; succ ; var = hfrom.var } p succ_n ]
        else 
        (* prem 4 of A3'''' *)
        let forall_p_neq_n_m_st_succ_p_neq_m = NodeSet.for_all (fun p -> p = n || p = succ_n || (SuccMap.find p hfrom.succ) <> succ_n) hfrom.nodes in
        let nodes = NodeSet.remove succ_n hfrom.nodes in
        let succ = SuccMap.add n 0 (project_onto hfrom.succ nodes) in
      
          if n <> 0 && succ_n <> 0 && succ_n <> n && forall_w_st_Vofw_neq_m && forall_p_neq_n_m_st_succ_p_neq_m then
            (* A9'''' *)
            [ Asgn (ctr_n, Num 1), {nodes ; succ ; var = hfrom.var } ]
        else
          assert false
  | NextAsgnId (u,w) -> (* A10 *)
      let n = VarMap.find u hfrom.var in
      let vw = VarMap.find w hfrom.var in
      let succ_n = SuccMap.find n hfrom.succ in
      let succ = SuccMap.add n vw hfrom.succ in
      assert ((n <> 0) && (succ_n = 0)) ;
      [ Assume True, { nodes = hfrom.nodes ; succ ; var = hfrom.var } ]
  | Assume g -> [ Assume g, hfrom ]
  | Atomic stmts  -> 
      Debugger.logf Debugger.Info "\n  ATOMIC TRANSITION COMPUTATION:\n  " ;
      let cfg = Cfg.ast_to_cfg stmts in
      (* don't prune infeasible edges on atomic transition computation;
       * otherwise we end up with an incorrect final vertex.
       * the infeasible stmt will later be reduced. *)
      let cfg' = convert ~indent:2 ~prune_infeasible:false hfrom cfg in
      let final_vertices = G.fold_vertex (fun v l -> match G.succ cfg' v with [] -> v :: l | _ -> l) cfg' [] in
      (* List.iter (fun (vertex,_) -> Printf.printf "FINAL VERTEX: %d\n" vertex) final_vertices ; *)
      let final_heaps = List.map (fun (ploc,heap) -> 
        let var = VarMap.filter (fun v n -> not (Str.string_match (Str.regexp "^__") v 0)) heap.var in
        { nodes = heap.nodes ; succ = heap.succ ; var }
      ) final_vertices in
      let paths = List.map (fun final -> Dijkstra.shortest_path cfg' (0, hfrom) final) final_vertices in
      let path_stmts_seq = List.map (fun (path,_) -> List.map (fun (from, (stmt,_), to_) -> stmt) path) paths in
      let rec unwrap_atomic seq = match seq with
        | Atomic s :: rest -> (unwrap_atomic s) @ (unwrap_atomic rest)
        | s1 :: rest -> s1 :: (unwrap_atomic rest)
        | [] -> []
      in
      let path_stmts_seq = List.map unwrap_atomic path_stmts_seq in
      (* List.iter (fun path -> Printf.printf "LEN PATH: %d\n" (List.length path)) path_stmts_seq ; *)
      (* List.iter (fun path -> print_endline (Ast.pprint_seq ~sep:"; " path)) path_stmts_seq ; *)
      List.map2 (fun stmt heap -> 
        (try reduce_seq_stmts stmt heap.nodes with Invalid_argument msg -> Atomic stmt),
        heap) path_stmts_seq final_heaps
  | _ -> assert false

let remove_summary_edges g =
  G.iter_edges_e (fun (f, e, t) -> let _, (_, (_, summary),_) = e in if summary > 0 then G.remove_edge_e g (f, e, t)) g
