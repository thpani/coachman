let fn_prog = Sys.argv.(1)
let fn_heap = Sys.argv.(2)
let fn_summary = Sys.argv.(3)

let position_string lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.sprintf "%d:%d" pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let () =
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
    let parsed = parse fn_heap Parser_heap.heap Lexer_heap.token in
    Heap.structure_from_heap parsed
  in
  let parse_program path =
    let ast_list = parse path Parser.program Lexer.token in
    List.map (fun (id, ast) -> id, Ast.precompile_seq ast) ast_list
  in
  let get_sccs g =
    let components = (Heap.SCC.scc_list g) in
    let non_trivial_sccs = List.filter (fun vertices -> (List.length vertices) > 1) components in
    List.length non_trivial_sccs
  in
  let print_stats g =
    Printf.printf "vertices: %d, edges: %d, SCCs: %d" (Heap.G.nb_vertex g) (Heap.G.nb_edges g) (get_sccs g)
  in
  let gen_cfg ast = Cfg.ast_to_cfg ~reduce:true ast in
  let init_heap = parse_heap in
  let ast_list = parse_program fn_prog in
  let ast_summ = parse_program fn_summary in
  List.iter (fun (fun_name, ast) ->
    let cfg = gen_cfg ast in
    Cfg.add_summaries cfg ast_summ ;
    Printf.printf "%s() :: " fun_name ;
    let cfg_dot = (Filename.basename Sys.argv.(1)) ^ "." ^ fun_name ^ ".dot" in
    let bicfg = Heap.convert init_heap cfg in
    let bicfg_dot = (Filename.basename Sys.argv.(1)) ^ "." ^ fun_name ^ ".bi.dot" in
    Cfg.Dot.write_dot cfg cfg_dot ;
    Heap.Dot.write_dot bicfg bicfg_dot ;
    print_stats bicfg ;
    Heap.remove_summary_edges bicfg ;
    Printf.printf ", SCCs (ranked): %d\n" (get_sccs bicfg)
  ) ast_list
