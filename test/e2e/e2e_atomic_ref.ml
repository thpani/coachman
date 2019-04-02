open OUnit2

open E2e

let suite = "AtomicRef" >::: [
  "|| (get [] set [] getAndSet)" >:: test "atomic_ref"
    "atomic_ref.tiny" "atomic_ref.heap" "atomic_ref/atomic_ref.summaries" "" [
      0, Scfg.effect_ID, 1, Complexity.Const 1;
      0, Scfg.effect_ID, 3, Complexity.Const 1;
      1, Scfg.effect_ID, 18, Complexity.Const 1;
      3, Scfg.effect_ID, 4, Complexity.Const 1;
      3, Scfg.effect_ID, 9, Complexity.Const 1;
      4, Scfg.effect_ID, 5, Complexity.Const 1;
      5, Scfg.effect_ID, 18, Complexity.Const 0;
      5, Scfg.E "Modify", 18, Complexity.Const 1;
      9, Scfg.effect_ID, 12, Complexity.Const 1;
      12, Scfg.effect_ID, 13, Complexity.Linear "N";
      13, Scfg.effect_ID, 12, Complexity.Linear "N";
      13, Scfg.E "Modify", 18, Complexity.Const 1;
    ] ;
]

