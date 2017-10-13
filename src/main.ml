let fn_prog = Sys.argv.(1)
let fn_heap = Sys.argv.(2)
let fn_summary = Sys.argv.(3)

let print_position lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.printf "%d:%d" pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let () =
  let parse_heap =
    let input = open_in fn_heap in
    let lexbuf = Lexing.from_channel input in
    let heap = Parser_heap.heap Lexer_heap.token lexbuf in
    let s = Heap.structure_from_heap heap in
    close_in input ; s
  in
  let parse_program path =
    let input = open_in path in
    let lexbuf = Lexing.from_channel input in
    try
      let ast = Parser.program Lexer.token lexbuf in
      let ast_precomp = Ast.precompile_seq ast in
      close_in input ;
      Some ast_precomp
    with
    | Lexer.Error msg ->
        prerr_endline ("[Lexer error] " ^ path ^ msg) ;
        None
    | Parsing.Parse_error -> begin
        prerr_endline ("[Parser error] " ^ path) ;
        print_position lexbuf ;
        None
      end
  in
  let print_stats g =
    let components = (Heap.SCC.scc_list g) in
    let non_trivial_sccs = List.filter (fun vertices -> (List.length vertices) > 1) components in
    Printf.printf "vertices: %d, edges: %d, SCCs: %d\n" (Heap.G.nb_vertex g) (Heap.G.nb_edges g) (List.length non_trivial_sccs)
  in
  let gen_cfg ast = Cfg.ast_to_cfg ~reduce:true ast in
  let init_heap = parse_heap in
  let ast = parse_program fn_prog in
  let ast_summ = parse_program fn_summary in
  match ast, ast_summ with
  | Some ast, Some ast_summ ->
    let cfg = gen_cfg ast in
    let cfg = Cfg.add_summaries cfg (List.hd ast_summ) in
    let cfg_dot = (Filename.basename Sys.argv.(1)) ^ ".dot" in
    let bicfg = Heap.convert init_heap cfg in
    let bicfg_dot = (Filename.basename Sys.argv.(1)) ^ ".bi.dot" in
    Cfg.Dot.write_dot cfg cfg_dot ;
    Heap.Dot.write_dot bicfg bicfg_dot ;
    Printf.printf "Stats :: " ; print_stats bicfg ;
    Heap.remove_summary_edges bicfg ;
    Printf.printf "Stats (ranked) :: " ; print_stats bicfg ;
  | _ -> ()
