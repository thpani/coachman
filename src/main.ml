let fn_prog = Sys.argv.(1)
let fn_heap = Sys.argv.(2)
let fn_summary = Sys.argv.(3)

let print_position lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.printf "%d:%d" pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let () =
  let parse_heap =
    let input = Batteries.open_in fn_heap in
    let filebuf = Batteries.Lexing.from_input input in
    let heap = Parser_heap.heap Lexer_heap.token filebuf in
    let s = Heap.structure_from_heap heap in
    Batteries.close_in input ; s
  in
  let parse_program path =
    let input = Batteries.open_in path in
    let filebuf = Batteries.Lexing.from_input input in
    try
      let ast = Parser.program Lexer.token filebuf in
      let ast_precomp = Ast.precompile_seq ast in
      Batteries.close_in input ;
      Some ast_precomp
    with
    | Lexer.Error msg ->
        prerr_endline ("[Lexer error] " ^ path ^ msg) ;
        None
    | Parsing.Parse_error -> begin
        prerr_endline ("[Parser error] " ^ path) ;
        print_position filebuf ;
        None
      end
  in
  let gen_cfg ast = Cfg.ast_to_cfg ~reduce:true ast in
  let init_heap = parse_heap in
  let ast = parse_program fn_prog in
  let ast_summ = parse_program fn_summary in
  match ast, ast_summ with
  | Some ast, Some ast_summ -> begin
      let cfg = gen_cfg ast in
      let cfg = Cfg.add_summaries cfg (List.hd ast_summ) in
      let chout = open_out ((Filename.basename Sys.argv.(1)) ^ ".dot") in
        Cfg.Dot.output_graph chout cfg ;
        close_out chout ;
      let bicfg = Heap.convert init_heap cfg in
      let chout = open_out ((Filename.basename Sys.argv.(1)) ^ ".bi.dot") in
      (* print_string (Ast.pprint_seq ast_summ) ; *)
      Heap.Dot.output_graph chout bicfg ; close_out chout
      ;
      Heap.G.iter_vertex (fun v -> Printf.printf "NODE: %s\n" (Heap.dump_cloc v)) bicfg ;
      Heap.G.iter_vertex (fun (v1,v2) -> 
        Heap.G.iter_vertex (fun (w1,w2) ->
          let v = v1,v2 in
          let w = w1,w2 in
          assert ((not (Heap.cloc_equal v w)) || (Hashtbl.hash v = Hashtbl.hash w))
        ) bicfg 
      ) bicfg ;
      Printf.printf "%d %d\n" (Heap.G.nb_vertex bicfg) (Heap.G.nb_edges bicfg)
    end
  | _ -> ()
