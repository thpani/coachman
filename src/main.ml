let fn_prog = Sys.argv.(1)
let fn_heap = Sys.argv.(2)
let fn_summary = Sys.argv.(3)

let position_string lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.sprintf "%d:%d" pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let set_debug_level () = match Sys.getenv_opt "DEBUG" with
  | Some lvl -> Debugger.current_level := (Debugger.lvl_of_string lvl)
  | None     -> ()

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
    Ca.structure_from_parsed_heaps parsed
  in
  let parse_program path = parse path Parser.program Lexer.token in
  let init_heaps = parse_heap in
  let functions = parse_program fn_prog in
  let summaries = parse_program fn_summary in
  init_heaps, functions, summaries

let process_function init_heaps summaries (fun_name, ast) = 
  print_endline fun_name ;
  let fn_prog_basename = Filename.basename fn_prog in
  let cfg_dot = Printf.sprintf "%s.%s.dot" fn_prog_basename fun_name in
  let ca_dot = Printf.sprintf "%s.%s.bi.dot" fn_prog_basename fun_name in

  let cfg = Cfg.from_ast ast in
  let cfg_with_summaries = Cfg.add_summaries cfg summaries in

  let ca = Ca.from_cfg init_heaps cfg in
  Ca.Dot.write_dot ca ca_dot ;

  Bound.compute_bounds cfg ca

let main () =
  let init_heaps, functions, summaries = parse_input () in
  List.iter (process_function init_heaps summaries) functions

let () =
  set_debug_level (); main ()
