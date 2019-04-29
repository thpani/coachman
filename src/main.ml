(** Return the current position in [lexbuf] as string. *)
let position_string lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.sprintf "%d:%d" pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

(** Parse the file at [path] using parser [p] and lexer [l]. *)
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

(** Parse the heap file at [path]. *)
let parse_heap path =
  let parsed = parse path Parser_heap.heaps Lexer_heap.token in
  Ca_seq.structure_from_parsed_heaps parsed

(** Parse the program file at [path]. *)
let parse_program path =
  parse path Parser.program Lexer.token

(** Print a list of strings concatenated by "||". *)
let pprint_summaries = function
  | [] -> "∅"
  | summaries ->
    let print_g (s,_) = Printf.sprintf "G(%s)" s in
    summaries |> List.map print_g |> String.concat " || "

(** Sequentialize the AST [ast] with stateless atomic summaries [summaries]. *)
let sequentialize ast summaries =
  let cfg                = Cfg.of_ast ast in
  let cfg_with_summaries = Cfg.add_summaries cfg summaries in

  let get_color edge_type = match edge_type with
    | Scfg.S name -> Util.Colormap.get_color (Util.List.find name (List.map fst summaries))
    | _ -> 0x000000
  in

  cfg_with_summaries, get_color

let main () =
  let args = ref [] in
  let func = ref "" in
  let usage = "Usage: coachman [args] <prog> <heap> <summaries>" in
  Arg.parse [
    "-debug", Arg.String (fun s ->
        Debugger.current_level := Debugger.Debug ;
        Debugger.current_components := (String.split_on_char ',' s)
      ), "Enable debugging" ;
    "-notime", Arg.Clear Debugger.print_time, "Omit elapsed time from log messages" ;
    "-ai", Arg.Set Config.use_ai, "Enable SCC pruning using abstract interpretation" ;
    "-no_iso", Arg.Clear Config.iso, "Disable matching isomorphic heaps" ;
    "-no_qflia", Arg.Clear Config.qf_lia, "Disable fixing Z3 to QF_LIA" ;
    "-no_qe", Arg.Clear Config.qe, "Disable quantifier elimination in transition relation computation" ;
    "-f", Arg.Set_string func , "Specify function to analyze. If not given, a program choosing non-deterministically among all declared functions will be generated."
  ] (fun s -> args := s :: !args) usage
  ;

  Format.set_margin 160 ;

  match !args with
  | [ fn_summary ; fn_heap ; fn_prog ] ->
    begin
      let fn_prog_basename = Filename.remove_extension (Filename.basename fn_prog) in
      let init_heaps = parse_heap fn_heap in
      let functions = parse_program fn_prog in
      let summaries = parse_program fn_summary in
      let fun_name, fun_ast = match !func with
        | "" -> (* build most general client *)
          "(" ^ (String.concat " [] " (List.map fst functions)) ^ ")",
          Ast.build_nondet_switch functions
        | fun_name ->
          fun_name, List.assoc fun_name functions
      in
          Debugger.info "main" "%s || %s\n" fun_name (pprint_summaries summaries) ;
          Config.dot_basename := fn_prog_basename ;
          let cfg_with_summaries, get_color = sequentialize fun_ast summaries in
          let edge_bound_map = Bound.compute_bounds ~get_edge_color:get_color init_heaps cfg_with_summaries in
          Debugger.info "bound" "Result of R/G transition bound analysis:\n" ;
          Bound.print_edge_bound_map edge_bound_map ;
          Bound.write_bound_dot edge_bound_map get_color cfg_with_summaries
        ;
        Debugger.info "main" "Time taken: %.0fs\n" (Sys.time ())
    end
  | _ -> prerr_endline usage ; exit 1
