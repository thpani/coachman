open OUnit2

open E2e

let suite = "Quadratic" >::: [
  "(procedure) || G(ModifyA) || G(ModifyB" >:: test "quadratic"
    "quadratic.tiny" "quadratic.heap" "quadratic/quadratic.summaries" "" [
    0, Scfg.effect_ID, 1, Complexity.Const 1 ;
    1, Scfg.effect_ID, 4, Complexity.Const 1 ;
    4, Scfg.effect_ID, 5, Complexity.Polynomial [ ("N", 2) ] ;
    5, Scfg.effect_ID, 4, Complexity.Polynomial [ ("N", 2) ] ;
    5, Scfg.E "ModifyA", 6, Complexity.Linear "N" ;
    6, Scfg.effect_ID, 7, Complexity.Linear "N" ;
    7, Scfg.effect_ID, 4, Complexity.Linear "N" ;
    7, Scfg.E "ModifyB", 3, Complexity.Const 1 ;
  ]
]
