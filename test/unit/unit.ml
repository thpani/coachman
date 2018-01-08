open Ca_sca
open Util

open Z3

open OUnit2

module SCA = struct
  (* Test abstraction of transition relations to SCA. *)

  let ctx = mk_context []
  let x = Arithmetic.Integer.mk_const_s ctx "x"
  let x' = Arithmetic.Integer.mk_const_s ctx "x'"
  let minus_one = Expr.mk_numeral_int ctx (-1) (Arithmetic.Integer.mk_sort ctx)
  let plus_one = Expr.mk_numeral_int ctx 1 (Arithmetic.Integer.mk_sort ctx)

  let test_sca transrel exp _ =
    let abstr = abstract ctx transrel 1 "x" in
    assert_equal abstr exp

  let suite = "SCA" >::: [
    "NonStrict" >:: test_sca (Boolean.mk_eq ctx x' x) NonStrict ;
    "Strict"    >:: test_sca (Boolean.mk_eq ctx x' (Arithmetic.mk_add ctx [x; minus_one])) Strict ;
    "DontKnow"  >:: test_sca (Boolean.mk_eq ctx x' (Arithmetic.mk_add ctx [x; plus_one])) DontKnow ;
  ]

end

module Graph = struct
  (* Test `all_paths' by building the asynchronous product of a single vertex
   * cfg with the summary given by `fpath`, `fname`.
   * We then precompile, construct the CA, and check whether this results in
   * `num_paths` summary edges. *)

  let test_summary_paths init_heap fpath fname num_paths _ =
    let summaries = Main.parse_program ("test/e2e/" ^ fpath) in
    let summary_ast = List.assoc fname summaries in
    let summary_cfg = Cfg.from_ast_seq summary_ast in
    let cfg = Cfg.G.add_edge_e Cfg.G.empty (0, (summary_cfg, Scfg.S fname), 1) in
    let summary_cfg_precomp = Cfg.precompile cfg in
    let ca_seq = Ca_seq.of_cfg summary_cfg_precomp (0, init_heap) in
    (* Ca_seq.G.iter_edges_e (fun (_,(stmts,_),_) -> Printf.printf "%s\n" (Ca_seq.pprint_seq ~sep:"; " stmts)) ca_seq ; *)
    assert_equal (Ca_seq.G.nb_edges ca_seq) num_paths

  let init_heap_treiber = {
    Ca_vertex.nodes = Ca_vertex.NodeSet.(empty |> add 1) ;
    Ca_vertex.succ = Ca_vertex.NodeMap.(empty |> add 1 0) ;
    Ca_vertex.var = Ca_vertex.VariableMap.(empty |> add "ToS" 1)
  }
  let init_heap_treiber_empty = {
    Ca_vertex.nodes = Ca_vertex.NodeSet.empty ;
    Ca_vertex.succ = Ca_vertex.NodeMap.empty ;
    Ca_vertex.var = Ca_vertex.VariableMap.(empty |> add "ToS" 0)
  }
  let init_heap_ms = {
    Ca_vertex.nodes = Ca_vertex.NodeSet.(empty |> add 1 |> add 2) ;
    Ca_vertex.succ = Ca_vertex.NodeMap.(empty |> add 1 0 |> add 2 1) ;
    Ca_vertex.var = Ca_vertex.VariableMap.(empty |> add "Tail" 1 |> add "Head" 2)
  }
  let init_heap_ms_heqt = {
    Ca_vertex.nodes = Ca_vertex.NodeSet.(empty |> add 1) ;
    Ca_vertex.succ = Ca_vertex.NodeMap.(empty |> add 1 0) ;
    Ca_vertex.var = Ca_vertex.VariableMap.(empty |> add "Tail" 1 |> add "Head" 1)
  }
  let init_heap_ms_lags = {
    Ca_vertex.nodes = Ca_vertex.NodeSet.(empty |> add 1 |> add 2) ;
    Ca_vertex.succ = Ca_vertex.NodeMap.(empty |> add 1 2 |> add 2 0) ;
    Ca_vertex.var = Ca_vertex.VariableMap.(empty |> add "Tail" 1 |> add "node" 2)
  }
  let suite = "Graph" >::: [ "all_paths" >::: [
    "treiber_push" >:: test_summary_paths init_heap_treiber "treiber/treiber.summaries" "push" 1 ;
    "treiber_pop" >:: test_summary_paths init_heap_treiber "treiber/treiber.summaries" "pop" 2 ;
    "treiber_pop" >:: test_summary_paths init_heap_treiber_empty "treiber/treiber.summaries" "pop" 0 ;
    "treiber_deq_deq" >:: test_summary_paths init_heap_ms "ms/ms_deq.summaries" "deq" 2 ;
    "treiber_deq_deq" >:: test_summary_paths init_heap_ms_heqt "ms/ms_deq.summaries" "deq" 0 ;
    "treiber_deq_deq_swing" >:: test_summary_paths init_heap_ms "ms/ms_deq.summaries" "deq_swing" 0 ;
    "treiber_deq_deq_swing" >:: test_summary_paths init_heap_ms_heqt "ms/ms_deq.summaries" "deq_swing" 2 ;
    "treiber_enq_enq" >:: test_summary_paths init_heap_ms "ms/ms_enq.summaries" "enq" 1 ;
    "treiber_enq_enq_swing" >:: test_summary_paths init_heap_ms "ms/ms_enq.summaries" "enq_swing" 2 ;
    "treiber_enq_enq_swing" >:: test_summary_paths init_heap_ms_lags "ms/ms_enq.summaries" "enq_swing" 4 ;
  ] ]
end

let () =
  let suite = "Unit" >::: [ SCA.suite ; Graph.suite ] in
  run_test_tt_main suite
