open Printf

type log_level = Error | Warn | Info | Debug
type component = string

let ord = function Error -> 50 | Warn  -> 40 | Info  -> 30 | Debug -> 20

let string_of_lvl = function
  | Error -> "error"
  | Warn -> "warn"
  | Info -> "info"
  | Debug -> "debug"

let lvl_of_string = function
  | "error" -> Error
  | "warn" -> Warn
  | "info" -> Info
  | "debug" -> Debug
  | _ -> raise (Invalid_argument "Invalid log level")

let current_level = ref Info
let current_components : component list ref = ref []

let logf lvl =
  if ord lvl >= ord !current_level then (
    printf "[%s] " (string_of_lvl lvl) ;
    printf
  ) else ifprintf stdout

let logc lvl component =
  if List.mem component !current_components then logf lvl
  else ifprintf stdout

(* http://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#eta_expansion *)
let error f = logf Error f
let warn  f = logf Warn  f
let info  f = logf Info  f
let debug f = logc Debug f
