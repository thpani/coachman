open OUnit2

let suite =
  "e2e tests" >::: [ 
    E2e_atomic_ref.suite ; E2e_spinlock.suite ;
    E2e_treiber.suite ; E2e_ms.suite ; E2e_dglm.suite
  ]

let () =
  Debugger.current_level := Debugger.Error ;
  run_test_tt_main suite
