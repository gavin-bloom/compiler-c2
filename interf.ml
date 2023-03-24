open X86
open Cfg

(* helper functions for RegSet *)

let subst_regset set mappings = 
  List.fold_left (fun set (x,y) -> 
      if RegSet.mem x set 
      then RegSet.add y (RegSet.remove x set) 
      else set
    ) set mappings 

(* Map/interference graph type *)

module RMap = Map.Make(
  struct
    type t = reg
    let compare = compare
  end
  )

type ifg = RegSet.t RMap.t

let regSetFmt s = RegSet.fold (fun x y -> (X86.reg_str x) ^ ", " ^ y) s "" 
let format (graph:ifg): string = 
  RMap.fold (fun x y z -> z^ "[" ^ (X86.reg_str x) ^ "]: " ^ (regSetFmt y) ^ "\n") graph ""


let neighbors reg graph = RMap.find reg graph
let n_neighbors reg graph = RegSet.cardinal (neighbors reg graph)

let add_edge reg1 reg2 graph =
  let update_add reg1 reg2 graph =
    match RMap.find_opt reg1 graph with
    | Some set -> RMap.add reg1 (RegSet.add reg2 set) graph
    | None -> RMap.add reg1 (RegSet.singleton reg2) graph
  in
  if reg1 = reg2 then (* don't allow self-edges - makes things simpler *)
    graph 
  else 
    let graph' = update_add reg1 reg2 graph in
    let graph'' = update_add reg2 reg1 graph'
    in 
    graph''

let add_edges reg edges graph =
  RegSet.fold (add_edge reg) edges graph

let add_node_edges reg edges graph = 
  let graph = 
    if RMap.mem reg graph then 
      graph 
    else 
      RMap.add reg RegSet.empty graph
  in add_edges reg edges graph


let add_all_mutual graph set = 
  RegSet.fold (fun el acc -> add_node_edges el set acc) set graph

let add_node reg graph = add_node_edges reg RegSet.empty graph

let remove_node reg graph =
  let graph = RMap.remove reg graph in
  RMap.map (RegSet.remove reg) graph
