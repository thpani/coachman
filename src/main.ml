let fn_prog = Sys.argv.(1)
let fn_heap = Sys.argv.(2)
let fn_summary = Sys.argv.(3)

let position_string lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.sprintf "%d:%d" pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let set_debug_level () = match Sys.getenv_opt "DEBUG" with
  | Some s ->
      Debugger.current_level := Debugger.Debug ;
      Debugger.current_components := (String.split_on_char ',' s)
  | None   -> ()

let parse_input () =
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
  in
  let parse_heap =
    let parsed = parse fn_heap Parser_heap.heaps Lexer_heap.token in
    Ca_seq.structure_from_parsed_heaps parsed
  in
  let parse_program path = parse path Parser.program Lexer.token in
  let init_heaps = parse_heap in
  let functions = parse_program fn_prog in
  let summaries = parse_program fn_summary in
  init_heaps, functions, summaries

let pprint_summaries summaries = String.concat " || " (List.map (fun s -> Printf.sprintf "G(%s)" (fst s)) summaries)

let process_function init_heaps summaries (fun_name, ast) = 
  Printf.printf "%s || %s\n" fun_name (pprint_summaries summaries) ;
  let fn_prog_basename = Filename.remove_extension (Filename.basename fn_prog) in
  let dot_basename = Printf.sprintf "%s.%s" fn_prog_basename fun_name in

  let cfg                = Cfg.from_ast ast in
  let cfg_with_summaries = Cfg.add_summaries cfg summaries in

  let get_color edge_type = match edge_type with
    | Scfg.S name -> Util.Colormap.get_color (Util.List.find name (List.map fst summaries))
    | _ -> 0x000000
  in
  (* let module CfgDot = Cfg.Dot(struct *)
  (*   let get_color (_,(_,et),_) = get_color et *)
  (*   let get_label (_,(stmts,_),_) = Cfg.pprint_seq stmts *)
  (* end) in *)
  (* CfgDot.write_dot cfg dot_basename "cfg" ; *)
  (* CfgDot.write_dot cfg_with_summaries dot_basename "cfg_summaries" ; *)

  Bound.compute_bounds ~dot_basename:dot_basename ~get_edge_color:get_color init_heaps cfg_with_summaries

let main () =
  let init_heaps, functions, summaries = parse_input () in
  List.iter (process_function init_heaps summaries) functions

let () =
  set_debug_level (); main ()
