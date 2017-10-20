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
    | Parser.Error -> Printf.eprintf "[Parser error] %s @ %s\n" path (position_string lexbuf) ; exit (1)
  in
  let parse_heap =
    let parsed = parse fn_heap Parser_heap.heap Lexer_heap.token in
    Ca.structure_from_parsed_heap parsed
  in
  let parse_program path = parse path Parser.program Lexer.token in
  let get_sccs g =
    let components = (Ca.SCC.scc_list g) in
    let non_trivial_sccs = List.filter (fun vertices -> (List.length vertices) > 1) components in
    List.length non_trivial_sccs
  in
  let print_stats g =
    Printf.printf "vertices: %d, edges: %d, SCCs: %d" (Ca.G.nb_vertex g) (Ca.G.nb_edges g) (get_sccs g)
  in
  let init_heap = parse_heap in
  let functions = parse_program fn_prog in
  let summaries = parse_program fn_summary in
  List.iter (fun (fun_name, ast) ->
    Printf.printf "%s() :: " fun_name ;
    let cfg = Cfg.from_ast ast in
    let cfg_dot = (Filename.basename Sys.argv.(1)) ^ "." ^ fun_name ^ ".dot" in
    Cfg.add_summaries cfg summaries ;
    Cfg.precompile cfg ;
    let ca = Ca.from_cfg init_heap cfg in
    let ca_dot = (Filename.basename Sys.argv.(1)) ^ "." ^ fun_name ^ ".bi.dot" in
    Cfg.Dot.write_dot cfg cfg_dot ;
    Ca.Dot.write_dot ca ca_dot ;
    print_stats ca ;
    Ca.remove_summary_edges ca ;
    Printf.printf ", SCCs (ranked): %d\n" (get_sccs ca)
  ) functions

let () =
  set_debug_level(); main()
