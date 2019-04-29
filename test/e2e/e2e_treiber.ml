open OUnit2

open E2e

let suite = "Treiber" >::: [
  "original" >::: [
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
    "(push [] pop) || G(push) || G(pop)" >:: test "treiber"
      "treiber.tiny" "treiber.heap" "treiber/treiber.summaries" "" [
        0, Scfg.effect_ID, 1, Complexity.Const 1 ;
        0, Scfg.effect_ID, 12, Complexity.Const 1 ;
        1, Scfg.effect_ID, 4, Complexity.Const 1 ;
        4, Scfg.effect_ID, 5, Complexity.Linear "N" ;
        5, Scfg.effect_ID, 6, Complexity.Linear "N" ;
        6, Scfg.effect_ID, 4, Complexity.Linear "N" ;
        6, Scfg.E "push", 21, Complexity.Const 1 ;
        12, Scfg.effect_ID, 13, Complexity.Linear "N" ;
        13, Scfg.effect_ID, 16, Complexity.Linear "N" ;
        16, Scfg.effect_ID, 17, Complexity.Linear "N" ;
        17, Scfg.effect_ID, 12, Complexity.Linear "N" ;
        13, Scfg.effect_ID, 21, Complexity.Const 1 ;
        17, Scfg.E "pop", 21, Complexity.Const 1 ;
    ] ;
  ] ; "partial" >::: [
        "push (empty)" >:: test "treiber"
      "treiber_partial.tiny" "treiber.heap" "empty.summaries" "push" [
        0, Scfg.effect_ID, 3, Complexity.Const 1 ;
        3, Scfg.effect_ID, 4, Complexity.Const 1 ;
        4, Scfg.effect_ID, 5, Complexity.Const 1 ;
        5, Scfg.effect_ID, 3, Complexity.Const 0 ;
        5, Scfg.E "push" , 2, Complexity.Const 1 ;
    ] ;
  "pop (empty)" >:: test "treiber"
    "treiber_partial.tiny" "treiber.heap" "empty.summaries" "pop" [
      0, Scfg.effect_ID, 3, Complexity.Unbounded ;
      3, Scfg.effect_ID, 0, Complexity.Unbounded ;
      3, Scfg.effect_ID, 6, Complexity.Const 1 ;
      6, Scfg.effect_ID, 7, Complexity.Const 1 ;
      7, Scfg.effect_ID, 0, Complexity.Const 0 ;
      7, Scfg.E "pop", 1, Complexity.Const 1 ;
    ] ;
  "(push [] pop) || G(push) || G(pop)" >:: test "treiber"
    "treiber_partial.tiny" "treiber.heap" "treiber/treiber.summaries" "" [
      0, Scfg.effect_ID, 1, Complexity.Const 1 ;
      0, Scfg.effect_ID, 12, Complexity.Const 1 ;
      1, Scfg.effect_ID, 4, Complexity.Const 1 ;
      4, Scfg.effect_ID, 5, Complexity.Linear "N" ;
      5, Scfg.effect_ID, 6, Complexity.Linear "N" ;
      6, Scfg.effect_ID, 4, Complexity.Linear "N" ;
      6, Scfg.E "push", 21, Complexity.Const 1 ;
      12, Scfg.effect_ID, 13, Complexity.Unbounded ;
      13, Scfg.effect_ID, 12, Complexity.Unbounded ;
      13, Scfg.effect_ID, 16, Complexity.Linear "N" ;
      16, Scfg.effect_ID, 17, Complexity.Linear "N" ;
      17, Scfg.effect_ID, 12, Complexity.Linear "N" ;
      17, Scfg.E "pop", 21, Complexity.Const 1 ;
    ]
  ]
]
