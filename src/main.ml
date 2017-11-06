let fn_prog = Sys.argv.(1)
let fn_heap = Sys.argv.(2)
let fn_summary = Sys.argv.(3)

let position_string lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.sprintf "%d:%d" pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let set_debug_level () =
  match Sys.getenv_opt "DEBUG" with
  | Some lvl ->
    Debugger.current_level := (Debugger.lvl_of_string lvl)
  | None -> ()

let main () =
  let parse path p l =
    let input = open_in path in
    let lexbuf = Lexing.from_channel input in
    try
      let parsed = p l lexbuf in
      close_in input ; parsed
    with
    | Lexer.Error msg -> Printf.eprintf "[Lexer error] %s @  %s: %s\n" path (position_string lexbuf) msg ; exit (1)
    | Parsing.Parse_error -> Printf.eprintf "[Parser error] %s @ %s\n" path (position_string lexbuf) ; exit (1)
  in
  let parse_heap =
    let parsed = parse fn_heap Parser_heap.heaps Lexer_heap.token in
    Ca.structure_from_parsed_heaps parsed
  in
  let parse_program path = parse path Parser.program Lexer.token in
  let get_sccs g =
    let components = (Ca.SCC.scc_list g) in
    let non_trivial_sccs = List.filter (fun vertices -> (List.length vertices) > 1) components in
    List.length non_trivial_sccs
  in
  let remove_non_scc g =
    let components = (Ca.SCC.scc_list g) in
    let scc_vertices = List.fold_left (fun l vertices -> if (List.length vertices) > 1 then vertices @ l else l) [] components in
    Ca.G.iter_vertex (fun v ->
      if not (List.exists (fun scc_vertex -> Ca.cloc_equal v scc_vertex) scc_vertices) then Ca.G.remove_vertex g v
    ) g
  in
  let print_stats g =
    Printf.printf "vertices: %d, edges: %d, SCCs: %d\n" (Ca.G.nb_vertex g) (Ca.G.nb_edges g) (get_sccs g)
  in
  let init_heaps = parse_heap in
  let functions = parse_program fn_prog in
  let summaries = parse_program fn_summary in
  List.iter (fun (fun_name, ast) ->
    let fn_prog_basename = Filename.basename fn_prog in
    let cfg_dot = Printf.sprintf "%s.%s.dot" fn_prog_basename fun_name in
    let ca_dot = Printf.sprintf "%s.%s.bi.dot" fn_prog_basename fun_name in
    let cfg = Cfg.from_ast ast in
    Cfg.add_summaries cfg summaries ;
    Cfg.precompile cfg ;
    Cfg.Dot.write_dot cfg cfg_dot ;

    let ca = Ca.from_cfg init_heaps cfg in
    Ca.Dot.write_dot ca ca_dot ;

    if (Filename.basename fn_prog) = "treiber.tiny" then begin
      Ca.remove_summary_edges ca ;
      assert ((get_sccs ca) = 0)
    end else begin
      print_stats ca ;

      let man, env, abs_map = Ai.do_abstract_computation_initial_values init_heaps ca in
      let num_inf = Ai.remove_infeasible man env abs_map ca in
      Printf.printf "Removing infeasible edges (%d)...\n" num_inf ;
      print_stats ca ;

      Ca.remove_summary_edges ~summary:(Some 1) ca ;
      Printf.printf "Removing summary edges [enq]...\n" ;
      print_stats ca ;

      let man, env, abs_map = Ai.do_abstract_computation man env abs_map ca in
      let num_inf = Ai.remove_infeasible man env abs_map ca in
      Printf.printf "Removing infeasible edges (%d)...\n" num_inf ;
      print_stats ca ;


      remove_non_scc ca ;
      Ca.Dot.write_dot ca ca_dot ;
      (* print_absv man abs_map ca; *)
      Printf.printf "\n" ;
    end
  ) functions

let () =
  set_debug_level(); main()
