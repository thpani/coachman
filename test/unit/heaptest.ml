open OUnit2

open Ast
open Heap

let nodes_empty = NodeSet.empty
let nodes_1 = NodeSet.singleton 1
let nodes_2 = NodeSet.singleton 2
let nodes_3 = NodeSet.singleton 3
let nodes_23 = NodeSet.add 3 nodes_2
let nodes_234 = NodeSet.add 4 nodes_23
let nodes_12 = NodeSet.add 2 nodes_1
let nodes_13 = NodeSet.add 3 nodes_1
let nodes_123 = NodeSet.add 3 nodes_12
let nodes_1234 = NodeSet.add 4 nodes_123
let succ_empty = SuccMap.empty
let succ_1_0 = SuccMap.add 1 0 succ_empty
let succ_1_1 = SuccMap.add 1 1 succ_empty
let succ_1_2 = SuccMap.add 1 2 succ_empty
let succ_1_2_0 = SuccMap.add 2 0 succ_1_2
let succ_1_2_2 = SuccMap.add 2 2 succ_1_2
let succ_1_3 = SuccMap.add 1 3 succ_empty
let succ_12_0 = SuccMap.add 2 0 succ_1_0
let succ_12_2 = SuccMap.add 2 2 succ_1_2
let succ_123_2 = SuccMap.add 3 2 succ_12_2
let succ_13_2 = SuccMap.add 3 2 succ_1_2
let succ_13_2_0 = SuccMap.add 2 0 succ_13_2
let succ_134_2 = SuccMap.add 4 2 succ_13_2
let succ_2_0 = SuccMap.add 2 0 succ_empty
let succ_2_1 = SuccMap.add 2 1 succ_empty
let succ_2_2 = SuccMap.add 2 2 succ_empty
let succ_23_0 = SuccMap.add 3 0 (SuccMap.add 2 0 succ_empty)
let succ_23_2 = SuccMap.add 3 2 (SuccMap.add 2 2 succ_empty)
let succ_3_2_0 = SuccMap.add 3 2 (SuccMap.add 2 0 succ_empty)
let succ_34_2 = SuccMap.add 3 2 (SuccMap.add 4 2 succ_empty)
let succ_1_0_23_2 = SuccMap.add 3 2 (SuccMap.add 2 2 succ_1_0)
let succ_1_0_34_2 = SuccMap.add 3 2 (SuccMap.add 4 2 succ_1_0)
let succ_13_0 = SuccMap.add 3 0 succ_1_0
let succ_3_0 = SuccMap.add 3 0 succ_empty
let var_u_0 = VarMap.add "u" 0 VarMap.empty
let var_u_1 = VarMap.add "u" 1 VarMap.empty
let var_u_2 = VarMap.add "u" 2 VarMap.empty
let var_u_0_w_1 = VarMap.add "w" 1 var_u_0
let var_uw_1 = VarMap.add "w" 1 var_u_1
let var_w_1_u_2 = VarMap.add "w" 1 var_u_2

let h_A1 = { nodes = nodes_empty ; succ = succ_empty ; var = var_u_0 }
let h_A2 = { nodes = nodes_1 ; succ = succ_empty ; var = VarMap.add "w" 1 var_u_1 }
let h_A2_res = { nodes = nodes_1 ; succ = succ_empty ; var = VarMap.add "w" 1 var_u_0 }
let h_A2'_1 = { nodes = nodes_123 ; succ = succ_13_2 ; var = var_u_2 }
let h_A2'_1_res = { nodes = nodes_123 ; succ = succ_13_2 ; var = var_u_0 }
let h_A2'_2 = { nodes = nodes_12 ; succ = succ_12_2 ; var = var_u_2 }
let h_A2'_2_res = { nodes = nodes_12 ; succ = succ_12_2 ; var = var_u_0 }
let h_A2'' = { nodes = nodes_12 ; succ = succ_1_2_0 ; var = var_u_2 }
let h_A2''_res = { nodes = nodes_1 ; succ = succ_1_0 ; var = var_u_0 }
let h_A3_1 = { nodes = nodes_1 ; succ = succ_1_0 ; var = var_u_1 }
let h_A3_1_res = { nodes = nodes_empty ; succ = succ_empty ; var = var_u_0 }
let h_A3_2 = { nodes = nodes_1 ; succ = succ_1_1 ; var = var_u_1 }
let h_A3_2_res = { nodes = nodes_empty ; succ = succ_empty ; var = var_u_0 }
let h_A3' = { nodes = nodes_12 ; succ = succ_1_2 ; var = VarMap.add "w" 2 var_u_1 }
let h_A3'_res = { nodes = nodes_2 ; succ = succ_empty ; var = VarMap.add "w" 2 var_u_0 }
let h_A3''_1 = { nodes = nodes_1234 ; succ = succ_134_2 ; var = var_u_1 }
let h_A3''_1_res = { nodes = nodes_234 ; succ = succ_34_2 ; var = var_u_0 }
let h_A3''_2 = { nodes = nodes_123 ; succ = succ_123_2 ; var = VarMap.add "p" 2 var_u_1 }
let h_A3''_2_res = { nodes = nodes_23 ; succ = succ_23_2 ; var = VarMap.add "p" 2 var_u_0 }
let h_A3''' = { nodes = nodes_123 ; succ = succ_13_2_0 ; var = var_u_1 }
let h_A3'''_res = { nodes = nodes_3 ; succ = succ_3_0 ; var = var_u_0 }
let h_A3'''' = { nodes = nodes_12 ; succ = succ_12_2 ; var = var_u_1 }
let h_A3''''_res = { nodes = nodes_empty ; succ = succ_empty ; var = var_u_0 }
let h_A4 = { nodes = nodes_1 ; succ = succ_1_0 ; var = var_u_0_w_1 }
let h_A4_res = { nodes = nodes_1 ; succ = succ_1_0 ; var = var_uw_1 }
let h_A5_res = { nodes = nodes_12 ; succ = succ_12_0 ; var = var_w_1_u_2 }
let h_A7_1_res = { nodes = nodes_1 ; succ = succ_1_0 ; var = var_u_0_w_1 }
let h_A7_2_res = { nodes = nodes_12 ; succ = succ_1_2_0 ; var = var_w_1_u_2 }
let h_A9_1 = { nodes = nodes_1 ; succ = succ_1_0 ; var = var_u_1 }
let h_A9_1_res = h_A9_1
let h_A9_2 = { nodes = nodes_1 ; succ = succ_1_1 ; var = var_u_1 }
let h_A9_2_res = { nodes = nodes_1 ; succ = succ_1_0 ; var = var_u_1 }
let h_A9' = { nodes = nodes_12 ; succ = succ_1_2 ; var = VarMap.add "w" 2 var_u_1 }
let h_A9'_res = { nodes = nodes_12 ; succ = succ_1_0 ; var = VarMap.add "w" 2 var_u_1 }
let h_A9''_1 = { nodes = nodes_1234 ; succ = succ_134_2 ; var = var_u_1 }
let h_A9''_1_res = { nodes = nodes_1234 ; succ = succ_1_0_34_2 ; var = var_u_1 }
let h_A9''_2 = { nodes = nodes_123 ; succ = succ_123_2 ; var = VarMap.add "p" 2 var_u_1 }
let h_A9''_2_res = { nodes = nodes_123 ; succ = succ_1_0_23_2 ; var = VarMap.add "p" 2 var_u_1 }
let h_A9''' = { nodes = nodes_123 ; succ = succ_13_2_0 ; var = var_u_1 }
let h_A9'''_res = { nodes = nodes_13 ; succ = succ_13_0 ; var = var_u_1 }
let h_A9'''' = { nodes = nodes_12 ; succ = succ_12_2 ; var = var_u_1 }
let h_A9''''_res = { nodes = nodes_1 ; succ = succ_1_0 ; var = var_u_1 }
let h_A10 = { nodes = nodes_12 ; succ = succ_2_0 ; var = var_w_1_u_2 }
let h_A10_res = { nodes = nodes_12 ; succ = succ_2_1 ; var = var_w_1_u_2 }

let _ =
  run_test_tt_main (
    "Heap">:::
      List.map
      (fun (title, arg, stmt, exp) ->
        title >:: (fun test_ctxt ->
          let res = l2ca arg stmt in
          assert (((List.length res) = (List.length exp)) && (List.length res) > 0 && (List.length res <= 2))
          ;
          if (List.length res) = 1 && (List.length exp) = 1 then
            begin
              let stmt_actual, heap_actual = List.hd res in
              let heap_exp, stmt_exp = List.hd exp in
                (* print_endline title ; dump_str arg ; dump_str heap_exp ; dump_str heap_actual ; print_endline (Ast.pprint stmt_actual) ; *)
                assert_equal true (NodeSet.equal heap_exp.nodes heap_actual.nodes) ?msg:(Some "nodes differ") ;
                assert_equal true (SuccMap.equal (=) heap_exp.succ heap_actual.succ) ?msg:(Some "succ differs") ;
                assert_equal true (VarMap.equal (=) heap_exp.var heap_actual.var) ?msg:(Some "vars differ") ;
                assert_equal stmt_exp stmt_actual ?msg:(Some "stmts differ")
            end
          ;
          if (List.length res) = 2 && (List.length exp) = 2 then
            begin
              let stmt_actual, heap_actual = List.nth res 1 in
              let heap_exp, stmt_exp = List.nth exp 1 in
                assert_equal true (NodeSet.equal heap_exp.nodes heap_actual.nodes) ?msg:(Some "nodes differ");
                assert_equal true (SuccMap.equal (=) heap_exp.succ heap_actual.succ) ?msg:(Some "succ differs");
                assert_equal true (VarMap.equal (=) heap_exp.var heap_actual.var) ?msg:(Some "vars differ");
                assert_equal stmt_exp stmt_actual ?msg:(Some "stmts differ")
            end
        )
    )
    [ 
      "A1",     h_A1,     AsgnNull "u", [ h_A1,         Assume True ] ;
      "A2",     h_A2,     AsgnNull "u", [ h_A2_res,     Assume True ] ;
      "A2'_1",  h_A2'_1,  AsgnNull "u", [ h_A2'_1_res,  Assume True ] ;
      "A2'_2",  h_A2'_2,  AsgnNull "u", [ h_A2'_2_res,  Assume True ] ;
      "A2''",   h_A2'',   AsgnNull "u", [ h_A2''_res,   Asgn("x_1", Add("x_1", Id "x_2")) ] ;
      "A3_1",   h_A3_1,   AsgnNull "u", [ h_A3_1_res,   Assume True ] ;
      "A3_2",   h_A3_2,   AsgnNull "u", [ h_A3_2_res,   Assume True ] ;
      "A3'",    h_A3',    AsgnNull "u", [ h_A3'_res,    Assume True ] ;
      "A3''_1", h_A3''_1, AsgnNull "u", [ h_A3''_1_res, Assume True ] ;
      "A3''_2", h_A3''_2, AsgnNull "u", [ h_A3''_2_res, Assume True ] ;
      "A3'''",  h_A3''',  AsgnNull "u", [ h_A3'''_res,  Asgn("x_3", Add("x_3", Id "x_2")) ] ;
      "A3''''", h_A3'''', AsgnNull "u", [ h_A3''''_res, Assume True ] ;
      "A4", h_A4, Asgn ("u", Id "w"),   [ h_A4_res,     Assume True ] ;
      "A5", h_A4, Alloc "u",   [ h_A5_res,     Asgn("x_2", Num 1) ] ;
      "A7", h_A4, AsgnNext ("u", "w"),  [ h_A7_1_res,   Assume (Eq ("x_1", Num 1)) ; h_A7_2_res, Atomic [ Assume (Gt ("x_1", Num 1)) ; Asgn ("x_2", Add("x_1", Num (-1))) ] ] ;
      "A9_1", h_A9_1, NextAsgnNull "u", [ h_A9_1_res,   Asgn ("x_1", Num 1) ] ;
      "A9_2", h_A9_2, NextAsgnNull "u", [ h_A9_2_res,   Asgn ("x_1", Num 1) ] ;
      "A9'",  h_A9',  NextAsgnNull "u", [ h_A9'_res,    Asgn ("x_1", Num 1) ] ;
      "A9''_1", h_A9''_1, NextAsgnNull "u", [ h_A9''_1_res,  Asgn ("x_1", Num 1) ] ;
      "A9''_2", h_A9''_2, NextAsgnNull "u", [ h_A9''_2_res,  Asgn ("x_1", Num 1) ] ;
      "A9'''", h_A9''', NextAsgnNull "u",   [ h_A9'''_res,   Atomic [ Asgn ("x_1", Num 1); Asgn ("x_3", Add("x_3", Id "x_2")) ] ] ;
      "A9''''", h_A9'''', NextAsgnNull "u", [ h_A9''''_res, Asgn ("x_1", Num 1) ] ;
      "A10", h_A10, NextAsgnId ("u", "w"),  [ h_A10_res,  Assume True ] ;
    ]
  )
