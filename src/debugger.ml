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
let print_time = ref true

let logf ?(comp=true) lvl component =
  if ord lvl >= ord !current_level && (ord lvl >= ord Info || List.mem component !current_components) then
    begin
      let longest_component = 
        let component_lengths = List.map String.length !current_components in
        let list_max = function [] -> 0 | x::xs -> List.fold_left max x xs in
        max 5 (list_max component_lengths)
      in
      let padding = max 0 (longest_component - (String.length component)) in
      if comp then 
        begin
          if !print_time then
            begin
              let now = Unix.localtime (Unix.time ()) in
              (* let date = sprintf "%d-%02d-%02d %02d:%02d:%02d" (now.tm_year+1900) (now.tm_mon+1) now.tm_mday now.tm_hour now.tm_min now.tm_sec in *)
              let date = sprintf "%02d:%02d:%02d" now.tm_hour now.tm_min now.tm_sec in
              printf "%s " date
            end ;
          printf "[%s] " component ;
          printf "%s" (String.make padding ' ')
        end ;
      printf
    end
  else ifprintf stdout

(* http://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#eta_expansion *)
let error f = logf Error f
let warn  f = logf Warn  f
let info  f = logf Info  f
let debug f = logf Debug f

let error_nocomp f = logf ~comp:false Error f
let warn_nocomp  f = logf ~comp:false Warn  f
let info_nocomp  f = logf ~comp:false Info  f
let debug_nocomp f = logf ~comp:false Debug f
