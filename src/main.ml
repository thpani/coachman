let position_string lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.sprintf "%d:%d" pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let parse path p l =
  let input = open_in path in
  let lexbuf = Lexing.from_channel input in
  try
    let parsed = p l lexbuf in
    close_in input ; parsed
  with
  | Lexer.Error msg ->
      Printf.eprintf "[Lexer error] %s @  %s: %s\n" path (position_string lexbuf) msg ;
      exit (1)
  | Parsing.Parse_error ->
      Printf.eprintf "[Parser error] %s @ %s\n" path (position_string lexbuf) ;
      exit (1)

let parse_heap path =
  let parsed = parse path Parser_heap.heaps Lexer_heap.token in
  Ca_seq.structure_from_parsed_heaps parsed

let parse_program path = 
  parse path Parser.program Lexer.token

let pprint_summaries summaries = String.concat " || " (List.map (fun s -> Printf.sprintf "G(%s)" (fst s)) summaries)

let sequentialize (fun_name, ast) summaries = 
  Debugger.info "bound" "%s || %s\n" fun_name (pprint_summaries summaries) ;
  Config.dot_basename := Printf.sprintf "%s.%s" !Config.dot_basename fun_name ;

  let cfg                = Cfg.of_ast ast in
  let cfg_with_summaries = Cfg.add_summaries cfg summaries in

  let get_color edge_type = match edge_type with
    | Scfg.S name -> Util.Colormap.get_color (Util.List.find name (List.map fst summaries))
    | _ -> 0x000000
  in

  cfg_with_summaries, get_color

let main () =
  let args = ref [] in
  let usage = "Usage: codscost [args] <prog> <heap> <summaries>" in
  Arg.parse [
    "-debug", Arg.String (fun s ->
      Debugger.current_level := Debugger.Debug ;
      Debugger.current_components := (String.split_on_char ',' s)
    ), "Enable debugging" ;
    "-ai", Arg.Set Config.use_ai, "Enable SCC pruning using abstract interpretation" ;
    "-no_qflia", Arg.Clear Config.qf_lia, "Disable fixing Z3 to QF_LIA" ;
    "-no_qe", Arg.Clear Config.qe, "Disable quantifier elimination in transition relation computation"
  ] (fun s -> args := s :: !args) usage
  ;

  Format.set_margin 160 ;

  match !args with
  | [ fn_summary ; fn_heap ; fn_prog ] ->
    let fn_prog_basename = Filename.remove_extension (Filename.basename fn_prog) in
    let init_heaps = parse_heap fn_heap in
    let functions = parse_program fn_prog in
    let summaries = parse_program fn_summary in
    List.iter (fun fct ->
      Config.dot_basename := fn_prog_basename ;
      let cfg_with_summaries, get_color = sequentialize fct summaries in
      let edge_bound_map = Bound.compute_bounds ~get_edge_color:get_color init_heaps cfg_with_summaries in
      Bound.print_edge_bound_map edge_bound_map ;
      Bound.write_bound_dot edge_bound_map get_color cfg_with_summaries
    ) functions
  | _ -> prerr_endline usage ; exit 1
