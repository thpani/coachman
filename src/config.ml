let dot_basename = ref ""
let qf_lia = ref true
let qe = ref true
let use_ai = ref false
let iso = ref true

let ctx = ref None

let get_ctx () = match !ctx with
  | None     -> let c = Z3.mk_context [] in ctx := Some c ; c
  | Some ctx -> ctx

let get_qf_lia () =
  if !qf_lia then
    Some (Z3.Symbol.mk_string (get_ctx ()) "QF_LIA")
  else
    None
