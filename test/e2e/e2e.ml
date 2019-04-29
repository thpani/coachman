open OUnit2

let concat_path a b c =
  Filename.concat (Filename.concat a b) c

let test ?(ai=false) ?(iso=true) component fn_prog fn_heap fn_summary fun_name exp _ =
  Config.use_ai := ai ;
  Config.iso := iso ;
  let fn_prog = concat_path "test/e2e" component fn_prog in
  let fn_heap = concat_path "test/e2e" component fn_heap in
  let fn_summary = Filename.concat "test/e2e" fn_summary in
  let init_heaps = Main.parse_heap fn_heap in
  let functions = Main.parse_program fn_prog in
  let summaries = Main.parse_program fn_summary in
  let prog = match fun_name with 
  | "" -> Ast.build_nondet_switch functions
  | fun_name -> List.assoc fun_name functions
  in
  let cfg_with_summaries, get_color = Main.sequentialize prog summaries in
  let edge_bound_map = Bound.compute_bounds ~get_edge_color:get_color init_heaps cfg_with_summaries in
  List.iter (fun (f,ek,t,exp) ->
    try
      let bound = Bound.CfgEdgeMap.find (f,ek,t) edge_bound_map in
      let complexity = Bound.to_complexity bound in
      let msg = Printf.sprintf "%s actual: %s != expected: %s"
        (Cfg.G.pprint_cfg_edge (f,ek,t))
        (Complexity.pprint complexity)
        (Complexity.pprint exp)
      in
      assert_equal ~msg:msg complexity exp
  with Not_found ->
    assert_failure (Cfg.G.pprint_cfg_edge (f,ek,t))
    )  exp
