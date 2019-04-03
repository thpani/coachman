open OUnit2

open E2e

let suite_ttas = "TTAS" >::: [
  "(lock [] unlock) || G(Lock) || G(Unlock)" >:: test "spinlock"
    "ttas.tiny" "spinlock.heap" "spinlock/spinlock.summaries" "" [
      0, Scfg.effect_ID, 3, Complexity.Const 1 ;
      0, Scfg.effect_ID, 10, Complexity.Const 1 ;
      3, Scfg.effect_ID, 3, Complexity.Unbounded ;
      3, Scfg.effect_ID, 4, Complexity.Linear "N" ;
      4, Scfg.effect_ID, 3, Complexity.Linear "N" ;
      4, Scfg.E "Lock", 14, Complexity.Const 1 ;
      10, Scfg.effect_ID, 14, Complexity.Const 0 ;
      10, Scfg.E "Unlock", 14, Complexity.Const 1 ;
    ] ;
]

let suite_tas = "TAS" >::: [
  "(lock [] unlock) || G(Lock) || G(Unlock)" >:: test "spinlock"
    "tas.tiny" "spinlock.heap" "spinlock/spinlock.summaries" "" [
      0, Scfg.effect_ID, 3, Complexity.Const 1 ;
      0, Scfg.effect_ID, 7, Complexity.Const 1 ;
      3, Scfg.effect_ID, 3, Complexity.Unbounded ;
      3, Scfg.E "Lock", 11, Complexity.Const 1 ;
      7, Scfg.effect_ID, 11, Complexity.Const 0 ;
      7, Scfg.E "Unlock", 11, Complexity.Const 1 ;
    ] ;
]

let suite = "Spinlock" >::: [ suite_tas ; suite_ttas ]
