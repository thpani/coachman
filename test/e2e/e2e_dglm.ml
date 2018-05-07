(* vim: set foldmethod=marker: *)
open OUnit2

open E2e

let suite = "DGLM" >::: [
  (* enq {{{ *)
  "enq (empty) nolag" >:: test "dglm"
    "dglm.tiny" "dglm_nolag.heap" "empty.summaries" "enq" [
       0, Scfg.effect_ID,      3, Complexity.Const 1 ;
       3, Scfg.effect_ID,      4, Complexity.Const 1 ;
       4, Scfg.effect_ID,      5, Complexity.Const 1 ;
       5, Scfg.effect_ID,      3, Complexity.Const 0 ;
       5, Scfg.effect_ID,      8, Complexity.Const 1 ;
       8, Scfg.effect_ID,      9, Complexity.Const 1 ;
       8, Scfg.effect_ID,     16, Complexity.Const 0 ;
       9, Scfg.effect_ID,      3, Complexity.Const 0 ;
       9, Scfg.E "enq",       10, Complexity.Const 1 ;
      10, Scfg.effect_ID,      2, Complexity.Const 0 ;
      10, Scfg.E "enq_swing",  2, Complexity.Const 1 ;
      16, Scfg.effect_ID,      3, Complexity.Const 0 ;
      16, Scfg.E "enq_swing",  3, Complexity.Const 0 ;
    ] ;
  "enq (empty)" >:: test "dglm"
    "dglm.tiny" "dglm.heap" "empty.summaries" "enq" [
       0, Scfg.effect_ID,      3, Complexity.Const 1 ;
       3, Scfg.effect_ID,      4, Complexity.Const 1 ;
       4, Scfg.effect_ID,      5, Complexity.Const 1 ;
       5, Scfg.effect_ID,      3, Complexity.Const 0 ;
       5, Scfg.effect_ID,      8, Complexity.Const 1 ;
       8, Scfg.effect_ID,      9, Complexity.Const 1 ;
       8, Scfg.effect_ID,     16, Complexity.Const 1 ;
       9, Scfg.effect_ID,      3, Complexity.Const 0 ;
       9, Scfg.E "enq",       10, Complexity.Const 1 ;
      10, Scfg.effect_ID,      2, Complexity.Const 0 ;
      10, Scfg.E "enq_swing",  2, Complexity.Const 1 ;
      16, Scfg.effect_ID,      3, Complexity.Const 0 ;
      16, Scfg.E "enq_swing",  3, Complexity.Const 1 ;
    ] ;
  "enq || G(enq) nolag" >: test_case ~length:Long (
    test ~ai:true "dglm"
    "dglm.tiny" "dglm_nolag.heap" "dglm/dglm.summaries" "enq" [
       0, Scfg.effect_ID,      3, Complexity.Const 1 ;
       3, Scfg.effect_ID,      4, Complexity.Linear "N" ;
       4, Scfg.effect_ID,      5, Complexity.Linear "N" ;
       5, Scfg.effect_ID,      3, Complexity.Linear "N" ;
       5, Scfg.effect_ID,      8, Complexity.Linear "N" ;
       8, Scfg.effect_ID,      9, Complexity.Linear "N" ;
       8, Scfg.effect_ID,     16, Complexity.Linear "N" ;
       9, Scfg.effect_ID,      3, Complexity.Linear "N" ;
       9, Scfg.E "enq",       10, Complexity.Const 1 ;
      10, Scfg.effect_ID,      2, Complexity.Const 1 ;
      10, Scfg.E "enq_swing",  2, Complexity.Const 1 ;
      16, Scfg.effect_ID,      3, Complexity.Linear "N" ;
      16, Scfg.E "enq_swing",  3, Complexity.Linear "N" ;
    ] ) ;
  "enq || G(enq)" >: test_case ~length:Long (
    test ~ai:true "dglm"
    "dglm.tiny" "dglm.heap" "dglm/dglm.summaries" "enq" [
       0, Scfg.effect_ID,      3, Complexity.Const 1 ;
       3, Scfg.effect_ID,      4, Complexity.Linear "N" ;
       4, Scfg.effect_ID,      5, Complexity.Linear "N" ;
       5, Scfg.effect_ID,      3, Complexity.Linear "N" ;
       5, Scfg.effect_ID,      8, Complexity.Linear "N" ;
       8, Scfg.effect_ID,      9, Complexity.Linear "N" ;
       8, Scfg.effect_ID,     16, Complexity.Linear "N" ;
       9, Scfg.effect_ID,      3, Complexity.Linear "N" ;
       9, Scfg.E "enq",       10, Complexity.Const 1 ;
      10, Scfg.effect_ID,      2, Complexity.Const 1 ;
      10, Scfg.E "enq_swing",  2, Complexity.Const 1 ;
      16, Scfg.effect_ID,      3, Complexity.Linear "N" ;
      16, Scfg.E "enq_swing",  3, Complexity.Linear "N" ;
    ] ) ;
  (* }}} *)
  (* deq {{{ *)
  "deq (empty) nolag" >:: test "dglm"
    "dglm.tiny" "dglm_nolag.heap" "empty.summaries" "deq" [
       0, Scfg.effect_ID,      3, Complexity.Const 1 ;
       3, Scfg.effect_ID,      4, Complexity.Const 1 ;
       4, Scfg.effect_ID,      0, Complexity.Const 0 ;
       4, Scfg.effect_ID,      7, Complexity.Const 1 ;
       7, Scfg.effect_ID,      1, Complexity.Const 1 ;
       7, Scfg.effect_ID,      9, Complexity.Const 1 ;
       9, Scfg.effect_ID,      0, Complexity.Const 0 ;
       9, Scfg.E "deq",       10, Complexity.Const 1 ;
      10, Scfg.effect_ID,     11, Complexity.Const 1 ;
      11, Scfg.effect_ID,      1, Complexity.Const 1 ;
      11, Scfg.effect_ID,     12, Complexity.Const 0 ;
      12, Scfg.effect_ID,      1, Complexity.Const 0 ;
      12, Scfg.E "deq_swing",  1, Complexity.Const 0 ;
    ] ;
  "deq (empty)" >:: test "dglm"
    "dglm.tiny" "dglm.heap" "empty.summaries" "deq" [
       0, Scfg.effect_ID,      3, Complexity.Const 1 ;
       3, Scfg.effect_ID,      4, Complexity.Const 1 ;
       4, Scfg.effect_ID,      0, Complexity.Const 0 ;
       4, Scfg.effect_ID,      7, Complexity.Const 1 ;
       7, Scfg.effect_ID,      1, Complexity.Const 1 ;
       7, Scfg.effect_ID,      9, Complexity.Const 1 ;
       9, Scfg.effect_ID,      0, Complexity.Const 0 ;
       9, Scfg.E "deq",       10, Complexity.Const 1 ;
      10, Scfg.effect_ID,     11, Complexity.Const 1 ;
      11, Scfg.effect_ID,      1, Complexity.Const 1 ;
      11, Scfg.effect_ID,     12, Complexity.Const 1 ;
      12, Scfg.effect_ID,      1, Complexity.Const 0 ;
      12, Scfg.E "deq_swing",  1, Complexity.Const 1 ;
    ] ;
  "deq || G(deq) nolag" >:: test "dglm"
    "dglm.tiny" "dglm_nolag.heap" "dglm/dglm.summaries" "deq" [
       0, Scfg.effect_ID,      3, Complexity.Linear "N" ;
       3, Scfg.effect_ID,      4, Complexity.Linear "N" ;
       4, Scfg.effect_ID,      0, Complexity.Linear "N" ;
       4, Scfg.effect_ID,      7, Complexity.Linear "N" ;
       7, Scfg.effect_ID,      1, Complexity.Const 1 ;
       7, Scfg.effect_ID,      9, Complexity.Linear "N" ;
       9, Scfg.effect_ID,      0, Complexity.Linear "N" ;
       9, Scfg.E "deq",       10, Complexity.Const 1 ;
      10, Scfg.effect_ID,     11, Complexity.Const 1 ;
      11, Scfg.effect_ID,      1, Complexity.Const 1 ;
      11, Scfg.effect_ID,     12, Complexity.Const 0 ;
      12, Scfg.effect_ID,      1, Complexity.Const 0 ;
      12, Scfg.E "deq_swing",  1, Complexity.Const 0 ;
    ] ;
  "deq || G(deq)" >:: test "dglm"
    "dglm.tiny" "dglm.heap" "dglm/dglm.summaries" "deq" [
       0, Scfg.effect_ID,      3, Complexity.Linear "N" ;
       3, Scfg.effect_ID,      4, Complexity.Linear "N" ;
       4, Scfg.effect_ID,      0, Complexity.Linear "N" ;
       4, Scfg.effect_ID,      7, Complexity.Linear "N" ;
       7, Scfg.effect_ID,      1, Complexity.Const 1 ;
       7, Scfg.effect_ID,      9, Complexity.Linear "N" ;
       9, Scfg.effect_ID,      0, Complexity.Linear "N" ;
       9, Scfg.E "deq",       10, Complexity.Const 1 ;
      10, Scfg.effect_ID,     11, Complexity.Const 1 ;
      11, Scfg.effect_ID,      1, Complexity.Const 1 ;
      11, Scfg.effect_ID,     12, Complexity.Const 1 ;
      12, Scfg.effect_ID,      1, Complexity.Const 1 ;
      12, Scfg.E "deq_swing",  1, Complexity.Const 1 ;
    ] ;
  (* }}} *)
]
