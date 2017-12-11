let dot_basename = ref ""
let qf_lia = ref true
let qe = ref true

let get_qf_lia ctx =
  if !qf_lia then
    Some (Z3.Symbol.mk_string ctx "QF_LIA")
  else
    None
