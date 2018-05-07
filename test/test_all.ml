open OUnit2

let suite =
  "Tests" >::: [ E2e_all.suite; Unit_all.suite ]

let () =
  Debugger.current_level := Debugger.Error ;
  run_test_tt_main suite
