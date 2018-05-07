(* vim: set foldmethod=marker: *)
open OUnit2

open E2e

let suite = "Michael-Scott" >::: [
  (* enq {{{ *)
  "enq (empty) nolag" >:: test "ms"
    "ms.tiny" "ms_nolag.heap" "empty.summaries" "enq" [
       0, Scfg.effect_ID,     3, Complexity.Const 1 ;
       3, Scfg.effect_ID,     4, Complexity.Const 1 ;
       4, Scfg.effect_ID,     5, Complexity.Const 1 ;
       5, Scfg.effect_ID,     3, Complexity.Const 0 ;
       5, Scfg.effect_ID,     8, Complexity.Const 1 ;
       8, Scfg.effect_ID,     9, Complexity.Const 1 ;
       8, Scfg.effect_ID,    16, Complexity.Const 0 ;
       9, Scfg.effect_ID,     3, Complexity.Const 0 ;
       9, Scfg.E "enq",      10, Complexity.Const 1 ;
      10, Scfg.E "enq_swing", 2, Complexity.Const 1 ;
      10, Scfg.effect_ID,     2, Complexity.Const 0 ;
      16, Scfg.E "enq_swing", 3, Complexity.Const 0 ;
      16, Scfg.effect_ID,     3, Complexity.Const 0 ;
    ] ;
  "enq (empty)" >:: test "ms"
    "ms.tiny" "ms.heap" "empty.summaries" "enq" [
       0, Scfg.effect_ID,     3, Complexity.Const 1 ;
       3, Scfg.effect_ID,     4, Complexity.Const 1 ;
       4, Scfg.effect_ID,     5, Complexity.Const 1 ;
       5, Scfg.effect_ID,     3, Complexity.Const 0 ;
       5, Scfg.effect_ID,     8, Complexity.Const 1 ;
       8, Scfg.effect_ID,     9, Complexity.Const 1 ;
       8, Scfg.effect_ID,    16, Complexity.Const 1 ;
       9, Scfg.effect_ID,     3, Complexity.Const 0 ;
       9, Scfg.E "enq",      10, Complexity.Const 1 ;
      10, Scfg.E "enq_swing", 2, Complexity.Const 1 ;
      10, Scfg.effect_ID,     2, Complexity.Const 0 ;
      16, Scfg.E "enq_swing", 3, Complexity.Const 1 ;
      16, Scfg.effect_ID,     3, Complexity.Const 0 ;
    ] ;
  "enq || G(enq) nolag" >: test_case ~length:Long (
    test ~ai:true "ms"
    "ms.tiny" "ms_nolag.heap" "ms/ms.summaries" "enq" [
       0, Scfg.effect_ID,     3, Complexity.Const 1 ;
       3, Scfg.effect_ID,     4, Complexity.Linear "N" ;
       4, Scfg.effect_ID,     5, Complexity.Linear "N" ;
       5, Scfg.effect_ID,     3, Complexity.Linear "N" ;
       5, Scfg.effect_ID,     8, Complexity.Linear "N" ;
       8, Scfg.effect_ID,     9, Complexity.Linear "N" ;
       8, Scfg.effect_ID,    16, Complexity.Linear "N" ;
       9, Scfg.effect_ID,     3, Complexity.Linear "N" ;
       9, Scfg.E "enq",      10, Complexity.Const 1 ;
      10, Scfg.E "enq_swing", 2, Complexity.Const 1 ;
      10, Scfg.effect_ID,     2, Complexity.Const 1 ;
      16, Scfg.E "enq_swing", 3, Complexity.Linear "N" ;
      16, Scfg.effect_ID,     3, Complexity.Linear "N" ;
    ] ) ;
  "enq || G(enq)" >: test_case ~length:Long (
    test ~ai:true "ms"
    "ms.tiny" "ms.heap" "ms/ms.summaries" "enq" [
       0, Scfg.effect_ID,     3, Complexity.Const 1 ;
       3, Scfg.effect_ID,     4, Complexity.Linear "N" ;
       4, Scfg.effect_ID,     5, Complexity.Linear "N" ;
       5, Scfg.effect_ID,     3, Complexity.Linear "N" ;
       5, Scfg.effect_ID,     8, Complexity.Linear "N" ;
       8, Scfg.effect_ID,     9, Complexity.Linear "N" ;
       8, Scfg.effect_ID,    16, Complexity.Linear "N" ;
       9, Scfg.effect_ID,     3, Complexity.Linear "N" ;
       9, Scfg.E "enq",      10, Complexity.Const 1 ;
      10, Scfg.E "enq_swing", 2, Complexity.Const 1 ;
      10, Scfg.effect_ID,     2, Complexity.Const 1 ;
      16, Scfg.E "enq_swing", 3, Complexity.Linear "N" ;
      16, Scfg.effect_ID,     3, Complexity.Linear "N" ;
    ] ) ;
  (* }}} *)
  (* deq {{{ *)
  "deq (empty) nolag" >:: test "ms"
    "ms.tiny" "ms_nolag.heap" "empty.summaries" "deq" [
       0, Scfg.effect_ID,     3, Complexity.Const 1 ;
       3, Scfg.effect_ID,     4, Complexity.Const 1 ;
       4, Scfg.effect_ID,     5, Complexity.Const 1 ;
       5, Scfg.effect_ID,     8, Complexity.Const 1 ;
       5, Scfg.effect_ID,     0, Complexity.Const 0 ;
       8, Scfg.effect_ID,     9, Complexity.Const 1 ;
       8, Scfg.effect_ID,    16, Complexity.Const 1 ;
       9, Scfg.effect_ID,     1, Complexity.Const 1 ;
       9, Scfg.effect_ID,    11, Complexity.Const 0 ;
      11, Scfg.effect_ID,     0, Complexity.Const 0 ;
      11, Scfg.E "deq_swing", 0, Complexity.Const 0 ;
      16, Scfg.effect_ID,     0, Complexity.Const 0 ;
      16, Scfg.E "deq",       1, Complexity.Const 1 ;
    ] ;
  "deq (empty)" >:: test "ms"
    "ms.tiny" "ms.heap" "empty.summaries" "deq" [
       0, Scfg.effect_ID,     3, Complexity.Const 1 ;
       3, Scfg.effect_ID,     4, Complexity.Const 1 ;
       4, Scfg.effect_ID,     5, Complexity.Const 1 ;
       5, Scfg.effect_ID,     8, Complexity.Const 1 ;
       5, Scfg.effect_ID,     0, Complexity.Const 0 ;
       8, Scfg.effect_ID,     9, Complexity.Const 1 ;
       8, Scfg.effect_ID,    16, Complexity.Const 1 ;
       9, Scfg.effect_ID,     1, Complexity.Const 1 ;
       9, Scfg.effect_ID,    11, Complexity.Const 1 ;
      11, Scfg.effect_ID,     0, Complexity.Const 0 ;
      11, Scfg.E "deq_swing", 0, Complexity.Const 1 ;
      16, Scfg.effect_ID,     0, Complexity.Const 0 ;
      16, Scfg.E "deq",       1, Complexity.Const 1 ;
    ] ;
  "deq || G(deq) nolag" >:: test "ms"
    "ms.tiny" "ms_nolag.heap" "ms/ms.summaries" "deq" [
       0, Scfg.effect_ID,     3, Complexity.Linear "N" ;
       3, Scfg.effect_ID,     4, Complexity.Linear "N" ;
       4, Scfg.effect_ID,     5, Complexity.Linear "N" ;
       5, Scfg.effect_ID,     8, Complexity.Linear "N" ;
       5, Scfg.effect_ID,     0, Complexity.Linear "N" ;
       8, Scfg.effect_ID,     9, Complexity.Const 1 ;
       8, Scfg.effect_ID,    16, Complexity.Linear "N" ;
       9, Scfg.effect_ID,     1, Complexity.Const 1 ;
       9, Scfg.effect_ID,    11, Complexity.Const 0 ;
      11, Scfg.effect_ID,     0, Complexity.Const 0 ;
      11, Scfg.E "deq_swing", 0, Complexity.Const 0 ;
      16, Scfg.effect_ID,     0, Complexity.Linear "N" ;
      16, Scfg.E "deq",       1, Complexity.Const 1 ;
    ] ;
  "deq || G(deq)" >:: test "ms"
    "ms.tiny" "ms.heap" "ms/ms.summaries" "deq" [
       0, Scfg.effect_ID,     3, Complexity.Linear "N" ;
       3, Scfg.effect_ID,     4, Complexity.Linear "N" ;
       4, Scfg.effect_ID,     5, Complexity.Linear "N" ;
       5, Scfg.effect_ID,     8, Complexity.Linear "N" ;
       5, Scfg.effect_ID,     0, Complexity.Linear "N" ;
       8, Scfg.effect_ID,     9, Complexity.Const 1 ;
       8, Scfg.effect_ID,    16, Complexity.Linear "N" ;
       9, Scfg.effect_ID,     1, Complexity.Const 1 ;
       9, Scfg.effect_ID,    11, Complexity.Const 1 ;
      11, Scfg.effect_ID,     0, Complexity.Const 1 ;
      11, Scfg.E "deq_swing", 0, Complexity.Const 1 ;
      16, Scfg.effect_ID,     0, Complexity.Linear "N" ;
      16, Scfg.E "deq",       1, Complexity.Const 1 ;
    ] ;
  (* }}} *)
]

