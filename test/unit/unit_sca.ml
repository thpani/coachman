open OUnit2

open Ca_sca
open Util.Z3

(* Test abstraction of transition relations to SCA. *)

let ctx = mk_context []
let x = Arithmetic.Integer.mk_const_s ctx "x"
let x' = Arithmetic.Integer.mk_const_s ctx "x'"
let minus_one = Expr.mk_numeral_int ctx (-1) (Arithmetic.Integer.mk_sort ctx)
let plus_one = Expr.mk_numeral_int ctx 1 (Arithmetic.Integer.mk_sort ctx)

let test_sca transrel exp _ =
  let abstr = abstract ctx transrel 1 "x" in
  assert_equal abstr exp

let suite = "SCA" >::: [
  "NonStrict" >:: test_sca (Boolean.mk_eq ctx x' x) NonStrict ;
  "Strict"    >:: test_sca (Boolean.mk_eq ctx x' (Arithmetic.mk_add ctx [x; minus_one])) Strict ;
  "DontKnow"  >:: test_sca (Boolean.mk_eq ctx x' (Arithmetic.mk_add ctx [x; plus_one])) DontKnow ;
]
