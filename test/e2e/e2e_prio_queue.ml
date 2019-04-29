open OUnit2

open E2e

let suite = "PriorityQueue" >::: [
  "(add [] removeMin) || G(Add1) || G(Add2) || G(Remove1) || G(Remove2)" >: test_case ~length:Long (
    test "prio_queue" "prio_queue.tiny" "prio_queue.heap"
    "prio_queue/prio_queue.summaries" "" [
      0, Scfg.effect_ID, 1, Complexity.Const 1 ;
      0, Scfg.effect_ID, 22, Complexity.Const 1 ;
      1, Scfg.effect_ID, 2, Complexity.Const 1 ;
      2, Scfg.effect_ID, 5, Complexity.Const 1 ;
      2, Scfg.effect_ID, 13, Complexity.Const 1 ;
      5, Scfg.effect_ID, 6, Complexity.Linear "N" ;
      6, Scfg.effect_ID, 7, Complexity.Linear "N" ;
      7, Scfg.effect_ID, 5, Complexity.Linear "N" ;
      7, Scfg.E "Add1", 41, Complexity.Const 1 ;
      13, Scfg.effect_ID, 14, Complexity.Linear "N" ;
      14, Scfg.effect_ID, 15, Complexity.Linear "N" ;
      15, Scfg.effect_ID, 13, Complexity.Linear "N" ;
      15, Scfg.E "Add2", 41, Complexity.Const 1 ;
      22, Scfg.effect_ID, 23, Complexity.Linear "N" ;
      23, Scfg.effect_ID, 26, Complexity.Linear "N" ;
      26, Scfg.effect_ID, 27, Complexity.Linear "N" ;
      27, Scfg.effect_ID, 22, Complexity.Linear "N" ;
      23, Scfg.effect_ID, 32, Complexity.Const 1 ;
      27, Scfg.E "Remove1", -1, Complexity.Const 1 ;
      32, Scfg.effect_ID, 33, Complexity.Linear "N" ;
      33, Scfg.effect_ID, 36, Complexity.Linear "N" ;
      36, Scfg.effect_ID, 37, Complexity.Linear "N" ;
      37, Scfg.effect_ID, 32, Complexity.Linear "N" ;
      33, Scfg.effect_ID, -1, Complexity.Const 1 ;
      37, Scfg.E "Remove2", -1, Complexity.Const 1 ;
    ] ) ;
]
