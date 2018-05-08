open OUnit2

let suite = "Unit tests" >::: [ Unit_sca.suite ; Unit_graph.suite ; Unit_l2ca.suite ]

let () =
  run_test_tt_main suite
