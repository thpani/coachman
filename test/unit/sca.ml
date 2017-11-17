open Util
open Ca_rel.Abstract

open OUnit2

open Z3


let ctx = mk_context []
let x = Arithmetic.Integer.mk_const_s ctx "x"
let x' = Arithmetic.Integer.mk_const_s ctx "x'"
let minus_one = Expr.mk_numeral_int ctx (-1) (Arithmetic.Integer.mk_sort ctx)
let plus_one = Expr.mk_numeral_int ctx 1 (Arithmetic.Integer.mk_sort ctx)

let _ = run_test_tt_main ( "SCA">:::
  List.map (fun (title, transrel, exp) ->
    title >:: (fun test_ctxt ->
      let abstr = abstract ctx transrel 1 "x" in
      assert_equal abstr exp
    )
  ) [
  "NonStrict", Boolean.mk_eq ctx x' x, NonStrict ;
  "Strict", Boolean.mk_eq ctx x' (Arithmetic.mk_add ctx [x; minus_one]), Strict ;
  "DontKnow", Boolean.mk_eq ctx x' (Arithmetic.mk_add ctx [x; plus_one]), DontKnow
  ]
)
