open Netlist_simulator
;;

let schedule_only = ref false
and number_steps = ref (-1)
and filename = ref "" 
and rom_filename = ref "" 
and ram_filename = ref "" in

(* Récupération des arguments *)
Arg.parse 
    [("-print", Arg.Set schedule_only, "Faire uniquement le scheduling"); 
    ("-n", Arg.Set_int number_steps, "Nombre d'étapes à simuler"); 
    ("-rom", Arg.Set_string rom_filename, "Fichier contenant la ROM");
    ("-ram", Arg.Set_string ram_filename, "Fichier contenant la RAM")] 
    (fun s -> filename := s)
    "";

let filename_sch = ((Filename.chop_suffix !filename ".net") ^ "_sch.net") in

(* Scheduling *)
let scheduled_netlist = 
  if !schedule_only || not(Sys.file_exists filename_sch) then (
    let sorted_netlist =
        (try Scheduler.schedule (Netlist.read_file !filename);
        with | Scheduler.Combinational_cycle -> Format.eprintf "Cycle combinatoire dans la netlist.@."; exit 2 )
    in
    let out = open_out filename_sch in
    Netlist_printer.print_program out sorted_netlist;
    close_out out;
    sorted_netlist
) else (
    Netlist.read_file filename_sch
)
in 

if not !schedule_only then begin
  let load_file f = (* Pour charger la ROM et la RAM d'un fichier *)
      let in_c = open_in f in
      let n = in_channel_length in_c in
      let s = Bytes.create n in
      really_input in_c s 0 n; close_in in_c; Bytes.to_string(s)
  in
  let s = (try load_file !rom_filename with Sys_error(s) -> "") in
  let rom = Array.init (String.length s) (fun i -> match s.[i] with '0' -> false | '1' -> true | _ -> assert false) in
  let s = (try load_file !ram_filename with Sys_error(s) -> "") in
  let ram = Array.init (String.length s) (fun i -> match s.[i] with '0' -> false | '1' -> true | _ -> assert false) in
  Netlist_simulator.simulator scheduled_netlist !number_steps rom ram
end