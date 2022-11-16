exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph =
    { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n :: g.g_nodes

let node_of_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  try
    let n1 = node_of_label g id1 in
    let n2 = node_of_label g id2 in
    n1.n_link_to   <- n2 :: n1.n_link_to;
    n2.n_linked_by <- n1 :: n2.n_linked_by
  with Not_found -> Format.eprintf "Tried to add an edge between non-existing nodes"; raise Not_found

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let check_ancestors node = 
  let rec browse ancestors = match ancestors with
  | [] -> true
  | x :: q -> x.n_mark = Visited && browse q
  in browse node.n_linked_by

let next_visit nodes =
  List.filter (fun n -> check_ancestors n && n.n_mark == NotVisited) nodes

let visit nodes =
  List.iter (fun n -> n.n_mark <- Visited) nodes


let rec all_nodes_visited nodes = match nodes with
  | [] -> true
  | x :: q -> x.n_mark = Visited && all_nodes_visited q

let has_cycle g = 
  let rec browse nodes = match next_visit nodes with
  | [] -> not (all_nodes_visited nodes)
  | l -> visit l; browse nodes
  in clear_marks g; browse g.g_nodes

let rec label_list nodes = match nodes with
  | [] -> []
  | x :: q -> x.n_label :: label_list q

let topological g = 
  let rec browse nodes = match next_visit nodes with
    | [] when not (all_nodes_visited nodes) -> raise Cycle
    | [] -> []
    | l -> visit l; (label_list l) @ (browse nodes)
  in clear_marks g; browse g.g_nodes