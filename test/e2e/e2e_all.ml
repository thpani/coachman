open OUnit2

let huge = ref true

let () =
  Debugger.current_level := Debugger.Error ;
  Arg.parse [
    "-no-huge", Arg.Clear huge , "Omit huge (<30min) test cases."
  ] (fun _ -> ()) "" ;
  let suite = "e2e" >::: [ 
    E2e_atomic_ref.suite ; E2e_spinlock.suite ;
    E2e_quadratic.suite ;
    E2e_treiber.suite ; E2e_ms.suite !huge ; E2e_dglm.suite !huge ;
    E2e_prio_queue.suite
  ] in
  run_test_tt_main suite
