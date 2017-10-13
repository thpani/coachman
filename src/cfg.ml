open Batteries
open Graph

open Ast

type ploc = int

module V_ = struct
  type t = ploc
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module E_ = struct
  type t = Ast.stmt * int
  let compare = compare
  let default = Assume True, 0
end

module G = Imperative.Digraph.ConcreteBidirectionalLabeled(V_)(E_)
module GChecker = Path.Check(G)

module Dot_ = Graphviz.Dot (struct
  include G
  let vertex_name v = "\"" ^ (string_of_int v) ^  "\""
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes (v1, (stmt, summary), v2) = [`Label (Ast.pprint ~atomic_angles:false stmt) ; `Color (if summary > 0 then 0xff0000 else 0)]
  let get_subgraph _ = None
end)

module Dot = struct
  include Dot_
  let write_dot g path = let chout = open_out path in Dot_.output_graph chout g ; close_out chout
end

let max_vertex g = G.fold_vertex (fun v a -> max v a) g 0
let next_vertex g = (max_vertex g) + 1

let rewrite_guard = function
  | Neg True -> False
  | Neg False -> True
  | Neg Eq (a,b) -> begin match b with
      | Id b when a = b -> False
      | _ -> Neg (Eq (a,b))
    end
  | Eq (a,b) -> begin match b with 
      | Id b when a = b -> True
      | _ -> Eq (a,b)
    end
  | g -> g

let rec reduce_cfg_assumes g =
  G.iter_edges_e (fun (from, (stmt, _), to_) -> 
      match stmt with
      | Assume False ->
          (* Printf.printf "rem1 %d -> %d\n" from to_ ; *)
          G.remove_edge g from to_
      | Assume True ->
          if from > 0 then
            let preds = G.pred_e g from in
            (* Printf.printf "rem2 %d -> %d\n" from to_ ; *)
            G.remove_edge g from to_ ;
            List.iter (
              fun (pred, s, pred_to) ->
                (* Printf.printf "rem3 %d -> %d\n" pred pred_to ; *)
                G.remove_edge g pred pred_to ;
                (* Printf.printf "add %d -> %d\n" pred to_ ; *)
                G.add_edge_e g (pred, s, to_) ;
              ) preds ;
          else ()
      | _ -> ()
  ) g

let reduce_cfg g =
  (* Rewrite CAS as atomic assume/assign *)
  (* Basic rewriting of guards (boolean simplification *)
  G.iter_edges_e (fun (from, (stmt, summary), to_) -> 
    match stmt with
    | Assume CAS (a,b,c) ->
        G.remove_edge g from to_ ;
        G.add_edge_e g (from, (Atomic [ Assume(Eq(a, Id b)) ; AsgnNull a ; Asgn(a, Id c) ], summary), to_)
    | Assume Neg CAS (a,b,c) ->
        G.remove_edge g from to_ ;
        G.add_edge_e g (from, (Atomic [ Assume(Neg(Eq(a,Id b))) ], summary), to_)
    | Assume guard ->
        G.remove_edge g from to_ ;
        G.add_edge_e g (from, (Assume(rewrite_guard guard), summary), to_)
    | _ -> ()
  ) g
  ;
  (* Remove assume(true) / assume(false) from the CFG *)
  reduce_cfg_assumes g ;
  (* Remove unreachable vertices *)
  G.iter_vertex (
    fun v -> if not (GChecker.check_path (GChecker.create g) 0 v) then
      G.remove_vertex g v) g
  ;
  g

let ast_to_cfg ?(reduce=true) ast = 
  let last = ref 0 in
  let break_target = ref 0 in
  let did_break = ref false in
  let g = G.create() in
  let rec x_ast_to_cfg ast =
    List.iter (fun stmt ->
      match stmt with
      | Ast.IfThenElse(guard, sif, selse) ->
          let sif = Ast.Assume(guard) :: sif in
          let selse = Ast.Assume(Ast.Neg(guard)) :: selse in
          let last_before_if = !last in
          let last_after_if = ref 0 in
            x_ast_to_cfg sif ;
            last_after_if := !last ;
            last := last_before_if ;
            x_ast_to_cfg selse ;
            let nv = if !did_break then next_vertex g else !last_after_if in
              G.add_edge_e g (!last, (Ast.Assume(Ast.True), 0), nv) ;
              did_break := false ;
              last := nv
      | Ast.While(guard, stmts) ->
          let before_branch = !last in
          let after_branch = next_vertex g in
          let stmts = Ast.Assume(guard) :: stmts in
          break_target := after_branch ;
          G.add_edge_e g (!last, (Ast.Assume(Ast.Neg(guard)), 0), after_branch) ;
          x_ast_to_cfg stmts ;
          G.add_edge_e g (!last, (Ast.Assume(Ast.True), 0), before_branch) ;
          last := max_vertex g
      | Ast.Break ->
          G.add_edge_e g (!last, (Ast.Assume(Ast.True), 0), !break_target) ;
          did_break := true
      | stmt ->
        let next_v = next_vertex g in
          G.add_edge_e g (!last, (stmt, 0), next_v) ; last := next_v
        )
    ast
  in
  x_ast_to_cfg ast ;
  if reduce then reduce_cfg g else g

let add_summaries cfg summary =
  G.iter_vertex (fun v -> G.add_edge_e cfg (v, (summary, 1), v)) cfg ; cfg
