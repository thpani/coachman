open Util
open Bound

open Z3

open OUnit2

let concat_path a b c =
  Filename.concat (Filename.concat a b) c

let test component fn_prog fn_heap fn_summary fun_name exp _ =
  let fn_prog = concat_path "test/e2e" component fn_prog in
  let fn_heap = concat_path "test/e2e" component fn_heap in
  let fn_summary = Filename.concat "test/e2e" fn_summary in
  let init_heaps, functions, summaries = Main.parse_input fn_prog fn_heap fn_summary in
  let prog = List.assoc fun_name functions in
  let cfg_with_summaries, get_color = Main.sequentialize (fun_name, prog) summaries in
  let edge_bound_map = compute_bounds ~get_edge_color:get_color init_heaps cfg_with_summaries in
  List.iter (fun (f,ek,t,exp) ->
    try
      let bound = CfgEdgeMap.find (f,ek,t) edge_bound_map in
      let complexity = bound_complexity bound in
      let msg = Printf.sprintf "%s actual: %s != expected: %s"
        (Cfg.G.pprint_cfg_edge (f,ek,t))
        (Complexity.pprint complexity)
        (Complexity.pprint exp)
      in
      assert_equal ~msg:msg complexity exp
    with Not_found ->
      assert_failure (Cfg.G.pprint_cfg_edge (f,ek,t))
  )  exp

let suite_treiber = "Treiber" >::: [
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
  "push || G(push) " >:: test "treiber"
    "treiber_push.tiny" "treiber.heap" "treiber/treiber.summaries" "push" [
      0, Scfg.effect_ID, 3, Complexity.Const 1 ;
      3, Scfg.effect_ID, 4, Complexity.Linear "N" ;
      4, Scfg.effect_ID, 5, Complexity.Linear "N" ;
      5, Scfg.effect_ID, 3, Complexity.Linear "N" ;
      5, Scfg.E "push" , 2, Complexity.Const 1 ;
    ] ;
  "pop || G(pop)" >:: test "treiber"
    "treiber_pop.tiny" "treiber.heap" "treiber/treiber.summaries" "pop" [
      0, Scfg.effect_ID, 3, Complexity.Linear "N" ;
      3, Scfg.effect_ID, 6, Complexity.Linear "N" ;
      6, Scfg.effect_ID, 7, Complexity.Linear "N" ;
      7, Scfg.effect_ID, 0, Complexity.Linear "N" ;
      3, Scfg.effect_ID, 1, Complexity.Const 1 ;
      7, Scfg.E "pop"  , 1, Complexity.Const 1 ;
    ] ;
]

let suite_ms = "Michael-Scott" >::: [
  "enq (empty) nolag" >:: test "ms"
    "ms.tiny" "ms_nolag.heap" "empty.summaries" "enq" [
       0, Scfg.effect_ID,     3, Complexity.Const 1 ;
       3, Scfg.effect_ID,     4, Complexity.Const 1 ;
       4, Scfg.effect_ID,     5, Complexity.Const 1 ;
       5, Scfg.effect_ID,     6, Complexity.Const 1 ;
       5, Scfg.effect_ID,    13, Complexity.Const 0 ;
      13, Scfg.effect_ID,     3, Complexity.Const 0 ;
      13, Scfg.E "enq_swing", 3, Complexity.Const 0 ;
       6, Scfg.effect_ID,     3, Complexity.Const 0 ;
       6, Scfg.E "enq",       7, Complexity.Const 1 ;
       7, Scfg.E "enq_swing", 2, Complexity.Const 1 ;
       7, Scfg.effect_ID,     2, Complexity.Const 0 ;
    ] ;
  "enq (empty)" >:: test "ms"
    "ms.tiny" "ms.heap" "empty.summaries" "enq" [
       0, Scfg.effect_ID,     3, Complexity.Const 1 ;
       3, Scfg.effect_ID,     4, Complexity.Const 1 ;
       4, Scfg.effect_ID,     5, Complexity.Const 1 ;
       5, Scfg.effect_ID,     6, Complexity.Const 1 ;
       5, Scfg.effect_ID,    13, Complexity.Const 1 ;
      13, Scfg.effect_ID,     3, Complexity.Const 0 ;
      13, Scfg.E "enq_swing", 3, Complexity.Const 1 ;
       6, Scfg.effect_ID,     3, Complexity.Const 0 ;
       6, Scfg.E "enq",       7, Complexity.Const 1 ;
       7, Scfg.E "enq_swing", 2, Complexity.Const 1 ;
       7, Scfg.effect_ID,     2, Complexity.Const 0 ;
    ] ;
  (* "enq || G(enq) nolag" >:: test "ms" *)
  (*   "ms.tiny" "ms_nolag.heap" "ms/ms_enq.summaries" "enq" [ *)
  (*      0, Scfg.effect_ID,     3, Complexity.Const 1 ; *)
  (*      3, Scfg.effect_ID,     4, Complexity.Linear "N" ; *)
  (*      4, Scfg.effect_ID,     5, Complexity.Linear "N" ; *)
  (*      5, Scfg.effect_ID,     6, Complexity.Linear "N" ; *)
  (*      5, Scfg.effect_ID,    13, Complexity.Linear "N" ; *)
  (*     13, Scfg.effect_ID,     3, Complexity.Linear "N" ; *)
  (*     13, Scfg.E "enq_swing", 3, Complexity.Linear "N" ; *)
  (*      6, Scfg.effect_ID,     3, Complexity.Linear "N" ; *)
  (*      6, Scfg.E "enq",       7, Complexity.Const 1 ; *)
  (*      7, Scfg.E "enq_swing", 2, Complexity.Const 1 ; *)
  (*      7, Scfg.effect_ID,     2, Complexity.Const 1 ; *)
  (*   ] ; *)
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
    "ms.tiny" "ms_nolag.heap" "ms/ms_deq.summaries" "deq" [
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
    "ms.tiny" "ms.heap" "ms/ms_deq.summaries" "deq" [
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
]

let () =
  Debugger.current_level := Debugger.Error ;
  run_test_tt_main suite_treiber ;
  run_test_tt_main suite_ms
