open Netlist_ast
open Graph

exception Combinational_cycle

let read_arg arg = match arg with
  | Avar x -> [x]
  | Aconst _ -> []

let read_exp eq = match eq with
  | (_, exp) -> match exp with
    | Earg a -> read_arg a
    | Ereg _ -> []
    | Enot a -> read_arg a
    | Ebinop (_, a1, a2) -> (read_arg a1) @ (read_arg a2)
    | Emux (a1, a2, a3) -> (read_arg a1) @ (read_arg a2) @ (read_arg a3)
    | Erom (_, _, a) -> read_arg a
    | Eram (_, _, a1, a2, a3, a4) -> []
    | Econcat (a1, a2) -> (read_arg a1) @ (read_arg a2)
    | Eslice (_,_,a) -> read_arg a
    | Eselect (_, a) -> read_arg a

let schedule p = 
  let g = mk_graph() in  
  let rec add_idents idents g = match idents with
    | [] -> ();
    | x :: q -> add_node g x; add_idents q g
  in add_idents p.p_inputs g; add_idents p.p_outputs g; 
  let idlist = let rec aux eql = match eql with
    | [] -> []
    | (x, _) :: q -> x :: (aux q) in aux p.p_eqs in add_idents idlist g;

  let rec add_edges g l id2 = match l with
    | [] -> ()
    | id1 :: q -> add_edge g id1 id2; add_edges g q id2
  in
  let schedule_eq g eq = match eq with
    | (x, _) -> add_edges g (read_exp eq) x 
  in
  let rec aux g p_list = match p_list with
    | [] -> ();
    | eq :: l -> schedule_eq g eq; aux g l
  in aux g p.p_eqs; 
  let rec get_eqs idl eqs = match idl with
    | [] -> []
    | x :: q -> 
      let rec find x eqs = match eqs with
        | (y, _) :: q when y != x -> find x q
        | (y, eq) :: q when y = x -> (x, eq) :: (find x q)
        | _ -> [] in
      find x eqs @ (get_eqs q eqs) in 
    { 
    p_eqs = get_eqs (try topological g with Cycle -> raise Combinational_cycle) p.p_eqs;
    p_inputs = p.p_inputs;
    p_outputs = p.p_outputs;
    p_vars = p.p_vars
    }
