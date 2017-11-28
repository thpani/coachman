(** Helper modules of utility definitions. *)

(** Extend module [List] with useful helper functions *)
module List = struct
  include List

  (** [List.find x [ a1; a2; ... ]] returns the position of the first element [ai] st [x = ai]. *)
  let find x lst = 
    let rec func x lst c = match lst with
      | hd :: tl -> if hd = x then c else func x tl (c+1)
      | [] -> failwith "Not Found"
    in func x lst 0
end

(** Extend module [Z3] with useful helper functions *)
module Z3 = struct
  include Z3

  let int_sort = Arithmetic.Integer.mk_sort

  (** [mk_numeral ctx n] makes a numeral of value [n]. *)
  let mk_numeral ctx n = Expr.mk_numeral_int ctx n (int_sort ctx)

  (** [mk_const ctx id] makes an integer constant of name [id]. *)
  let mk_const ctx id = Arithmetic.Integer.mk_const_s ctx id

  (** [mk_const' ctx num_primes id] makes an integer constant with name [id^(num_primes)], e.g., [mkconst' ctx 4 "a" -> "a''''"]. *)
  let mk_const' ctx num_primes id =
    let id' = id ^ (String.make num_primes '\'') in
    mk_const ctx id'
  
  (** [check ctx f] checks [f] for validity. *)
  let check_valid ctx f =
    let sat_problem = Boolean.mk_not ctx f in
    let s = Solver.mk_solver ctx None in
    Solver.add s [ sat_problem ] ;
    (Solver.check s []) = Solver.UNSATISFIABLE
end

(** Map integers to a set of colors. *)
module Colormap = struct
  type t = Scfg.edge_kind -> Graph.Graphviz.color

  (* [get_color i] maps [i] to a color. *)
  let get_color i =
    let colors = [ 0xff0000 ; 0x0000ff ; 0x00ff00 ; 0xff00ff ; 0xffff00 ; 0x00ffff ] in
    List.nth colors (i mod (List.length colors))
end

(** Set and map over identifiers. *)
module DS = struct
  type identifier = string

  module Identifier = struct 
    type t = identifier
    let compare = Pervasives.compare
  end
  module IdentifierSet = struct
    include Set.Make(Identifier)
    let pprint s = "{" ^ (String.concat "," (elements s)) ^ "}"
  end
  module IdentifierMap = struct
    include Map.Make(Identifier)
    let pprint_binding (v,n) = Printf.sprintf "%s:%d" v n
    let pprint m =
      let bindings_string = String.concat "," (List.map pprint_binding (bindings m)) in
      Printf.sprintf "{%s}" bindings_string
  end

end
