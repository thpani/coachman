open Printf

type log_level = Error | Warn | Info

let ord = function Error -> 50 | Warn  -> 40 | Info  -> 30

let string_of_lvl = function
  | Error -> "error"
  | Warn -> "warn"
  | Info -> "info"

let current_level = ref (ord Warn)

let logf lvl =
  if ord lvl >= !current_level then printf else ifprintf stdout
