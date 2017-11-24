type var_abs_map = Apron.Interval.t Cavertex.VariableMap.t
type ca_loc_with_constraints = Cavertex.ca_loc * var_abs_map

val compute_bounds : string -> (Scfg.edge_kind -> Graph.Graphviz.color) -> ca_loc_with_constraints list -> Cfg.G.t -> unit
