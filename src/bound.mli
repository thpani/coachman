(** Bound computation *)

type t = Bound of Z3.Expr.expr | Unbounded

module CfgEdge : Map.OrderedType with type t = Cfg.G.vertex * Scfg.edge_kind * Cfg.G.vertex

module CfgEdgeMap : Map.S with type key = CfgEdge.t

type cfg_edge_map = t CfgEdgeMap.t

type var_abs_map = Apron.Interval.t Ca_vertex.VariableMap.t
(** Map of variables to intervals. *)

type ca_loc_with_constraints = Ca_vertex.ca_loc * var_abs_map
(** Pair of CA vertex and associated map of variables to intervals. *)

val compute_bounds : ?get_edge_color:Util.Colormap.t -> ca_loc_with_constraints list -> Cfg.G.t -> cfg_edge_map
(** [compute_bounds initial_locs_with_constraints cfg] computes bounds on [cfg] with initial locations given by [inital_locs_with_constraints]
 
    [~get_edge_color] defines an optional color map, mapping edge types to colors. *)

val print_edge_bound_map : cfg_edge_map -> unit

val write_bound_dot : cfg_edge_map -> Util.Colormap.t -> Cfg.G.t -> unit

(* this is here for test/e2e/e2e.ml *)
val to_complexity : t -> Complexity.t
