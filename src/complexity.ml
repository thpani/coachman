type t = Const of int | Linear of string | Polynomial of (string * int) list | Unbounded | Unknown


let ord = function
  | Const _      -> 10
  | Linear _     -> 20
  | Polynomial _ -> 30
  | Unbounded    -> 99
  | Unknown      -> failwith "Unknown is greater AND smaller than all others, thus has no ord"

let pprint = function
  | Const i      -> Printf.sprintf "O(%d)" i
  | Linear s     -> Printf.sprintf "O(%s)" s
  | Polynomial l -> (match l with [ (id, pow) ] -> Printf.sprintf "O(%s^%d)" id pow | _ -> "???")
  | Unbounded    -> "∞"
  | Unknown      -> "???"

let compare a b op = match (a,b) with
| (Unknown,_) -> Unknown
| (_,Unknown) -> Unknown
| _ ->
    if op (ord a) (ord b) then a
    else if op (ord b) (ord a) then b
    else failwith "not implemented"

let max a b = compare a b (>)
let min a b = compare a b (<)

module VariableMap = Util.DS.IdentifierMap

let string_of_z3_symbol e =
  let open Z3 in
  Symbol.get_string (FuncDecl.get_name (Expr.get_func_decl e))

(* Conversion from Z3 expressions {{{ *)

let of_z3_mul e =
  assert (Z3.Arithmetic.is_mul e) ;
  let mul_args = Z3.Expr.get_args e in
  let constants = match mul_args with
    | first_arg :: tail_arg ->
        if Z3.Expr.is_numeral first_arg then
          tail_arg
        else
          mul_args
    | [] -> assert false
  in
  let occ_map = List.fold_left (fun map symbol ->
    let id = string_of_z3_symbol symbol in
    let occ = match VariableMap.find_opt id map with Some i -> i+1 | None -> 1 in
    VariableMap.add id occ map
  ) VariableMap.empty constants
  in
  if (VariableMap.cardinal occ_map) = 1 then
    let var, exp = VariableMap.min_binding occ_map in
    if exp = 1 then Linear var
    else Polynomial (VariableMap.bindings occ_map)
  else
    Polynomial (VariableMap.bindings occ_map)

let rec of_z3_expr e =
  let ctx = Config.get_ctx () in
  let params = Z3.Params.mk_params ctx in
  Z3.Params.add_bool params (Z3.Symbol.mk_string ctx "som") true ;
  let e = Z3.Expr.simplify e (Some params) in
  if Z3.Expr.is_numeral e then
    Const (Pervasives.min (Z3.Arithmetic.Integer.get_int e) 1)
  else if Z3.Expr.is_const e then
    Linear (string_of_z3_symbol e)
  else if Z3.Arithmetic.is_add e then
    of_z3_add e
  else if Z3.Arithmetic.is_mul e then
    of_z3_mul e
  else if Z3.Boolean.is_ite e then
    of_z3_ite e
  else begin
    Debugger.warn "complexity_asymp" "Unknown expr: %s\n" (Z3.Expr.to_string e) ;
    Unknown
  end
and of_z3_ite e =
  assert (Z3.Boolean.is_ite e) ;
  let ite_args = Z3.Expr.get_args e in
  let comparison, term1, term2 = List.nth ite_args 0, List.nth ite_args 1, List.nth ite_args 2 in
  assert (Z3.Arithmetic.is_le comparison) ;
  let comparison_args = Z3.Expr.get_args comparison in
  let comparison_arg1, comparison_arg2 = List.nth comparison_args 0, List.nth comparison_args 1 in
  let minmax_op = 
    if Z3.Expr.is_numeral comparison_arg1 && Z3.Expr.is_const comparison_arg2 then
      max
    else if Z3.Expr.is_const comparison_arg1 && Z3.Expr.is_numeral comparison_arg2 then
      min
    else
      assert false (* Complexity.Unknown *)
  in
  minmax_op (of_z3_expr term1) (of_z3_expr term2)
and of_z3_add e =
  assert (Z3.Arithmetic.is_add e) ;
  let args = Z3.Expr.get_args e in
  let last_arg = List.hd (List.rev args) in
  (* e is in sum-of-monomials format, so this is the highest degree term *)
  of_z3_expr last_arg

(* }}} *)
