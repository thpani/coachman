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
  "enq || G(enq) nolag" >:: test ~ai:true "ms"
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
    ] ;
  "enq || G(enq)" >:: test ~ai:true "ms"
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
    ] ;
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
  "deq || G(deq)" >:: test ~iso:false "ms"
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
  (* enq [] deq {{{ *)
  "(enq [] deq) || G(deq) || G(enq)" >:
		test_case ~length:Long (test "ms"
    "ms.tiny" "ms.heap" "ms/ms.summaries" "" [
      0, Scfg.effect_ID, 1, Complexity.Const 1 ;
      0, Scfg.effect_ID, 24, Complexity.Const 1 ;
      1, Scfg.effect_ID, 4, Complexity.Const 1 ;
      4, Scfg.effect_ID, 5, Complexity.Linear "N" ;
      5, Scfg.effect_ID, 6, Complexity.Linear "N" ;
      6, Scfg.effect_ID, 4, Complexity.Linear "N" ;
      6, Scfg.effect_ID, 9, Complexity.Linear "N" ;
      9, Scfg.effect_ID, 10, Complexity.Linear "N" ;
      9, Scfg.effect_ID, 17, Complexity.Linear "N" ;
      10, Scfg.effect_ID, 4, Complexity.Linear "N" ;
      10, Scfg.E "enq", 11, Complexity.Const 1 ;
      11, Scfg.effect_ID, 43, Complexity.Const 1 ;
      11, Scfg.E "enq_swing", 43, Complexity.Const 1 ;
      17, Scfg.effect_ID, 4, Complexity.Linear "N" ;
      17, Scfg.E "enq_swing", 4, Complexity.Linear "N" ;
      24, Scfg.effect_ID, 25, Complexity.Linear "N" ;
      25, Scfg.effect_ID, 26, Complexity.Linear "N" ;
      26, Scfg.effect_ID, 27, Complexity.Linear "N" ;
      27, Scfg.effect_ID, 24, Complexity.Linear "N" ;
      27, Scfg.effect_ID, 30, Complexity.Linear "N" ;
      30, Scfg.effect_ID, 31, Complexity.Linear "N" ;
      30, Scfg.effect_ID, 38, Complexity.Linear "N" ;
      31, Scfg.effect_ID, 33, Complexity.Linear "N" ;
      31, Scfg.effect_ID, 43, Complexity.Const 1 ;
      33, Scfg.effect_ID, 24, Complexity.Linear "N" ;
      33, Scfg.E "deq_swing", 24, Complexity.Linear "N" ;
      38, Scfg.effect_ID, 24, Complexity.Linear "N" ;
      38, Scfg.E "deq", 43, Complexity.Const 1 ;
  ] ) ;
  (* }}} *)
]

