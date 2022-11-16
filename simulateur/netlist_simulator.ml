open Netlist
open Netlist_ast
open Scheduler
let print_only = ref false

let number_steps = ref 3

(* Fonctions utiles *)
let value env arg = match arg with
| Avar(ident) -> Env.find ident env
| Aconst(v) -> v
let bit env arg = match value env arg with
| VBit(b) -> b
| _ -> failwith "Not a bit"
let bit_array env arg = match value env arg with
| VBitArray(ba) -> ba
| _ -> failwith "Not a bit array"
let rec int_of_value value = match value with (* utilisé pour la simulation de la ROM *)
| VBit(b) -> int_of_value (VBitArray([|b|]))
| VBitArray(ba) -> Array.fold_left (fun acc b -> 2*acc + (if b then 1 else 0)) 0 ba
let string_of_bool b = if b then "1" else "0"
let print_value env ident =
Format.printf "%s : %s \n" ident (match Env.find ident env with
  | VBit(b) -> string_of_bool b
  | VBitArray(ba) -> Array.fold_left (fun s b -> (string_of_bool b)^s) "" ba
)

let simulator program number_steps rom ram = 
  (* On stocke la valeur de chaque identifiant dans var_env *)
  let var_env = ref Env.empty in let prev_env = ref Env.empty in let step = ref 0 in let number_steps = ref number_steps in let ram_buffer = ref [] in let rom = ref rom in let ram = ref ram in
  while !number_steps > 0 do
    number_steps := !number_steps - 1;
    step := !step + 1;
    Format.printf "Step %d \n" !step;

  (* Lire les inputs *)

  let read_input ident =
    let rec query () =
      try
        Format.printf "%s ? @?" ident;
        match read_int() with
        | 0 -> false
        | 1 -> true
        | _ -> failwith "Invalid_input"
      with _ -> (print_endline "Invalid input"; query ())
    in query ()
  in

  var_env := List.fold_left (fun e ident ->
    Env.add ident (match Env.find ident program.p_vars with
        | TBit -> VBit(read_input ident)
        | TBitArray(l) -> VBitArray(Array.init l (fun i -> read_input (ident^"["^(string_of_int i)^"]")))
    ) e
  ) !var_env program.p_inputs;

  (* Évaluation d'une expression *)

  ram_buffer := [];
  let evaluate prev_env env exp = match exp with (* On garde l'environnement au cycle précédent pour la gestion des registres *)
    | Earg(arg) -> value env arg
    | Ereg(ident) -> 
      begin try value prev_env (Avar(ident)) with Not_found -> (match Env.find ident program.p_vars with
        | TBit -> VBit(false)
        | TBitArray(l) -> VBitArray(Array.make l false)
      ) end
    | Enot(arg) -> VBit(not(bit env arg))
    | Ebinop(op, a1, a2) ->
      let bitop b1 b2 = (match op with
        | Or -> b1 || b2
        | Xor -> b1 <> b2
        | And -> b1 && b2
        | Nand -> not(b1 && b2)
      ) in
      let bitwise_op bitop ba1 ba2 =
        let ba3 = Array.make (Array.length ba1) false in
        for i = 0 to (Array.length ba1) - 1 do
            ba3.(i) <- bitop ba1.(i) ba2.(i)
        done;
        ba3
      in
      (match (value env a1, value env a2) with
          | (VBit(b1), VBit(b2)) -> VBit(bitop b1 b2)
          | (VBitArray(ba1), VBitArray(ba2)) -> VBitArray(bitwise_op bitop ba1 ba2)
          | _ -> failwith "Arguments are not of the same type (bit or bit array)")
    | Emux(cond, a1, a2) -> if bit env cond then value env a1 else value env a2
    | Erom(_, word_size, read_addr) -> 
      let read_id = word_size * (int_of_value (value env read_addr)) in
      if word_size = 1 then VBit(!rom.(read_id)) else VBitArray(Array.sub !rom read_id word_size)
    | Eram(_, word_size, read_addr, write_enable, _, _) ->
      let read_id = word_size * (int_of_value (value env read_addr)) in
      if read_id + word_size > (Array.length !ram) then (
        ram := Array.init (read_id + word_size) (fun i -> try !ram.(i) with Invalid_argument(_) -> false)
      );
      if bit env write_enable then ram_buffer := exp :: !ram_buffer;
      if word_size = 1 then VBit(!ram.(read_id)) else VBitArray(Array.sub !ram read_id word_size)
    | Econcat(a1, a2) -> begin match (value env a1, value env a2) with
      | (VBitArray(ba1), VBitArray(ba2)) -> VBitArray(Array.concat [ba1; ba2])
      | (VBitArray(ba1), VBit(b2)) -> VBitArray(Array.concat [ba1; [|b2|]])
      | (VBit(b1), VBitArray(ba2)) -> VBitArray(Array.concat [[|b1|]; ba2])
      | (VBit(b1), VBit(b2)) -> VBitArray([|b1; b2|]) end
    | Eslice(i1, i2, arg) -> VBitArray(Array.sub (bit_array env arg) i1 (i2 - i1 + 1))
    | Eselect(i, arg) -> match value env arg with 
      | VBit(b) -> VBit(b)
      | VBitArray(ba) -> VBit(ba.(i))
    in

  (* Simulation des équations *)

  prev_env := !var_env;
  List.iter (fun (ident, exp) -> var_env := Env.add ident (evaluate !prev_env !var_env exp) !var_env) program.p_eqs;
  
  (* Écriture dans la RAM *)

  List.iter (function
    | Eram(_, word_size, _, write_enable, write_addr, data) -> 
      if bit !var_env write_enable then (
        let write_id = word_size * (int_of_value (value !var_env write_addr)) in
        if word_size = 1 then
          !ram.(write_id) <- (try bit !var_env data with Not_found -> false)
        else
          Array.iteri (fun i b -> !ram.(write_id + i) <- b) (try bit_array !var_env data with Not_found -> Array.make word_size false) 
      )
    | _ -> failwith "Pas une equation de RAM"
      ) !ram_buffer;

  (* Écriture des outputs *)
  
  List.iter (fun ident -> print_value !var_env ident) program.p_outputs; done