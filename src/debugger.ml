open Printf

type log_level = Error | Warn | Info

let ord = function Error -> 50 | Warn  -> 40 | Info  -> 30

let string_of_lvl = function
  | Error -> "error"
  | Warn -> "warn"
  | Info -> "info"

let lvl_of_string = function
  | "error" -> Error
  | "warn" -> Warn
  | "info" -> Info
  | _ -> raise (Invalid_argument "Invalid log level")

let current_level = ref Warn

let logf lvl component =
  if component = "bound" then
    if ord lvl >= ord !current_level then printf else ifprintf stdout
  else ifprintf stdout
