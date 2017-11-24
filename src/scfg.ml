open Graph

type ploc = int

module type GConfig = sig
  type vertex
  type edge_label

  val compare_vertex : vertex -> vertex -> int
  val hash_vertex    : vertex -> int
  val equal_vertex   : vertex -> vertex -> bool
  val pprint_vertex  : vertex -> string
  val get_ploc       : vertex -> ploc

  val compare_edge_label : edge_label -> edge_label -> int
  val equal_edge_label   : edge_label -> edge_label -> bool
  val default_edge_label : edge_label
  val pprint_edge_label  : edge_label -> string

  val color_edge        : edge_label -> Graphviz.color
end

type edge_kind = E of string | S of string
let pprint_edge_kind = function
  | E id -> Printf.sprintf "Effect %s" id
  | S id -> Printf.sprintf "Summary %s" id
let effect_ID = let effect_ID_id = "ID" in E effect_ID_id

module G (C:GConfig) = struct
  module V_ = struct
    type t = C.vertex
    let compare = C.compare_vertex
    let hash    = C.hash_vertex
    let equal   = C.equal_vertex
  end

  module E_ = struct
    type t = C.edge_label * edge_kind
    let compare (e1,ek1) (e2,ek2) =
      if ek1 = ek2 then
        C.compare_edge_label e1 e2
      else 
        Pervasives.compare e1 e2
    let default = C.default_edge_label, effect_ID
  end

  module G_ = Persistent.Digraph.ConcreteBidirectionalLabeled(V_)(E_)

  include G_
  include Components.Make(G_)

  module Imp = Imperative.Digraph.ConcreteBidirectionalLabeled(V_)(E_)

  module Dot = Graphviz.Dot (struct
    include G_
    let graph_attributes _ = []
    let default_vertex_attributes _ = [`Regular false]
    let vertex_name v = C.pprint_vertex v
    let vertex_attributes _ = []
    let get_subgraph _ = None
    let default_edge_attributes _ = []
    let edge_attributes (v,(e,ek),v') = [
      `Label (C.pprint_edge_label e) ;
      `Color (C.color_edge e) ;
      `Fontcolor (C.color_edge e)
    ]
  end)

  let pprint_cfg_edge (v,(l,et),v') = Printf.sprintf "%s: %d -> %d"
    (pprint_edge_kind et) (C.get_ploc v) (C.get_ploc v')

  let pprint_edge (v,(l,et),v') = Printf.sprintf "%s: %s -> %s"
    (pprint_cfg_edge (v,(l,et),v')) (C.pprint_vertex v) (C.pprint_vertex v')

  let equal_edge (c1,(l1,et1),c1') (c2,(l2,et2),c2') =
    C.equal_vertex c1 c2 && C.equal_vertex c1' c2' && C.equal_edge_label l1 l2 && et1 = et2

  let scc_edges g = 
    let scc_list = scc_list g in
    List.map (fun scc_vertices ->
      (* for this SCC (SCC = list of vertices) ... *)
      List.fold_left (fun l scc_vertex ->
        (* and this vertex, get all edges to successors that are also in the same SCC *)
        let scc_vertex_outgoing_edges_in_scc = List.fold_left (fun l edge ->
          let _, _, to_ = edge in
          let to_in_same_scc = List.exists (C.equal_vertex to_) scc_vertices in
          if to_in_same_scc then edge :: l else l
        ) [] (succ_e g scc_vertex)
        in
        scc_vertex_outgoing_edges_in_scc @ l
      ) [] scc_vertices
    ) scc_list

  let edge_in_scc edge scc =
    let g = List.fold_left add_edge_e empty scc in
    let scc_edges = List.concat (scc_edges g) in
    List.exists (equal_edge edge) scc_edges

  let scc_of_cfg_edge g f t et =
    let scc = List.filter (fun scc_edges ->
      List.exists (fun (v,(_,e),v') -> (C.get_ploc v)=f && (C.get_ploc v')=t && e = et) scc_edges
    ) (scc_edges g) in
    match scc with
    | [] -> empty
    | [ edges ] -> List.fold_left (fun g edge -> add_edge_e g edge) empty edges
    | _ -> assert false
    
end
