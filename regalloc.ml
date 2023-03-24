open Cfg
open Interf
open Spill
open X86


(* these are written in an order that 'makes sense' 
   meaning that they start with the caller-save non-args,
   then the args in reverse order, then the callee-saved
*)
let physical_registers = [ RAX; R11; R10; R9; R8; RCX; RDX; RSI; RDI; RBX; R12; R13; R14; R15 ]

let is_physical = function
  | Var _ -> false 
  | _ -> true

(* We always have all physical registers and always maximally interfere *)
let starter_graph = add_all_mutual RMap.empty (RegSet.of_list physical_registers)

let node_interf n = 
  match n.stmt with
  | Memory Move _ -> (n.live_out -- n.use) ++ n.def
  | _ -> n.live_out ++ n.def

let build cfg =
  let interferences = map_nodes cfg (fun n -> [node_interf n]) in
  let graph = List.fold_left add_all_mutual starter_graph interferences in
  remove_node RBP graph (* we don't want RBP in our graph *)


(* coloring code *)

(* We use 'this graph is entirely physical registers' as a proxy for easy colorability. *)
let is_simply_colorable graph = RMap.for_all (fun x _ -> is_physical x) graph 

(* all of this 'find' functions are pretty gross *)
(* find the node with the most neighbors - used in spilling *)
let find_largest graph =
  let sizes = RMap.bindings (RMap.map RegSet.cardinal graph)
  in 
  fst (List.hd (List.sort (fun x y -> snd y - snd x) sizes))

(* find a binding with the most neighbors < 14 - don't consider physical registers *)
let find_removable graph = 
  let sizes = RMap.bindings (RMap.map RegSet.cardinal graph) in
  (* remove physical registers and those with too many neighbors *)
  let available = List.filter (fun (r, n) -> n < 14 && not (is_physical r)) sizes in
  (* return highest degree option *)
  List.nth_opt (List.sort (fun x y -> snd x - snd y) available) 0

(* given a register, the current set of mappings, and a graph, find an
   acceptible color for this register *)
let find_color r mappings graph = 
  if is_physical r then r (* physical registers are pre-colored to themselves *)
  else (* find highest priority that isn't a neighbor. This is why physical_registers is ordered *)
    let n = neighbors r graph in
    let n = subst_regset n mappings (* re-write neighbors with mappings *)
    in
    List.hd (List.filter (fun x -> not (RegSet.mem x n)) physical_registers)

(* part one checks if colorable and if not, removes a node and recurses *)
let rec try_color (graph: ifg) (removed: (reg*RegSet.t) list) : (reg*reg) list option = 
  if is_simply_colorable graph 
  then (* we can start re-adding nodes *)
    Some (complete_coloring graph [] removed)
  else (* We have to keep removing nodes *)
    match find_removable graph with
    | None -> None (* No available node to remove. This will cause spilling *)
    | Some (node, _) -> (* We can remove a node and loop *)
      let rem = (node, neighbors node graph) in
      let graph = remove_node node graph 
      in
      try_color graph (rem::removed)

(* part two builds a mapping by re-adding nodes *)
and complete_coloring (graph: ifg) (mappings: (reg*reg) list) (removed: (reg*RegSet.t) list) : (reg*reg) list = 
  match removed with
  | [] -> (* None to add back, we have a mapping *)
    mappings
  | (node, edges) :: removed -> 
    (* Note by construction that if we've made it here, these all must work *)
    let graph = add_node_edges node edges graph in
    let dest = find_color node mappings graph 
    in 
    complete_coloring graph ((node, dest) :: mappings) removed

let rec regalloc (cfg:cfg) : cfg =
  let cfg = liveness cfg in (* compute liveness *)
  let interf = build cfg in (* build cfg *)
  (* let interf, rewriting = coalesce interf in (* coalesce - not sure about how to do this or if it is necessary *) *)
  match try_color interf [] with
  | Some s -> subst_cfg s cfg (* mapping found *)
  | None -> (* no mapping found, spill largest register and loop *)
    let r = find_largest interf in
    regalloc (spill r cfg)
