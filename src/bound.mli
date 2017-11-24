(** Bound computation *)

(** Map of variables to intervals. *)
type var_abs_map = Apron.Interval.t Ca_vertex.VariableMap.t

(** Pair of CA vertex and associated map of variables to intervals. *)
type ca_loc_with_constraints = Ca_vertex.ca_loc * var_abs_map

(** [compute_bounds initial_locs_with_constraints cfg] computes bounds on [cfg] with initial locations given by [inital_locs_with_constraints]
 
    [~dot_basename] gives an optional name for outputting [.dot] files.
    [~get_edge_color] defines an optional color map, mapping edge types to colors. *)
val compute_bounds : ?dot_basename:string -> ?get_edge_color:Util.Colormap.t -> ca_loc_with_constraints list -> Cfg.G.t -> unit
