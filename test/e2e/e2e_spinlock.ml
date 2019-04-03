open OUnit2

open E2e

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

let suite = "Spinlock" >::: [ suite_tas ]
