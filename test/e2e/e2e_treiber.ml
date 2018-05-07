open OUnit2

open E2e

let suite = "Treiber" >::: [
  "push (empty)" >:: test "treiber"
    "treiber.tiny" "treiber.heap" "empty.summaries" "push" [
      0, Scfg.effect_ID, 3, Complexity.Const 1 ;
      3, Scfg.effect_ID, 4, Complexity.Const 1 ;
      4, Scfg.effect_ID, 5, Complexity.Const 1 ;
      5, Scfg.effect_ID, 3, Complexity.Const 0 ;
      5, Scfg.E "push" , 2, Complexity.Const 1 ;
    ] ;
  "pop (empty)" >:: test "treiber"
    "treiber.tiny" "treiber.heap" "empty.summaries" "pop" [
      0, Scfg.effect_ID, 3, Complexity.Const 1 ;
      3, Scfg.effect_ID, 6, Complexity.Const 1 ;
      6, Scfg.effect_ID, 7, Complexity.Const 1 ;
      7, Scfg.effect_ID, 0, Complexity.Const 0 ;
      3, Scfg.effect_ID, 1, Complexity.Const 1 ;
      7, Scfg.E "pop"  , 1, Complexity.Const 1 ;
    ] ;
  "push || G(push)" >:: test "treiber"
    "treiber.tiny" "treiber.heap" "treiber/treiber.summaries" "push" [
      0, Scfg.effect_ID, 3, Complexity.Const 1 ;
      3, Scfg.effect_ID, 4, Complexity.Linear "N" ;
      4, Scfg.effect_ID, 5, Complexity.Linear "N" ;
      5, Scfg.effect_ID, 3, Complexity.Linear "N" ;
      5, Scfg.E "push" , 2, Complexity.Const 1 ;
    ] ;
  "pop || G(pop)" >:: test "treiber"
    "treiber.tiny" "treiber.heap" "treiber/treiber.summaries" "pop" [
      0, Scfg.effect_ID, 3, Complexity.Linear "N" ;
      3, Scfg.effect_ID, 6, Complexity.Linear "N" ;
      6, Scfg.effect_ID, 7, Complexity.Linear "N" ;
      7, Scfg.effect_ID, 0, Complexity.Linear "N" ;
      3, Scfg.effect_ID, 1, Complexity.Const 1 ;
      7, Scfg.E "pop"  , 1, Complexity.Const 1 ;
    ] ;
]
