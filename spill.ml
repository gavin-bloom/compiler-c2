open X86
open Cfg
open List

(* Spill code based primarily on class work *)

let find_defs r cfg = 
  map_nodes cfg (fun n -> if RegSet.mem r n.def then [n] else [])

let find_uses r cfg =
  map_nodes cfg (fun n -> if RegSet.mem r n.use then [n] else [])

let insert_after node cfg parent = 
  let node = { node with
               label = new_label cfg
             ; control = parent.control
             ; succ = parent.succ
             } 
  in
  let parent = { parent with 
                 control = Goto node.label
               ; succ = [node.label]
               }
  in
  let nodes = IMap.add parent.label parent cfg.nodes in
  let nodes = IMap.add node.label node nodes 
  in
  { cfg with nodes = nodes }

let insert_before node cfg original =
  let dest = original.label in
  let child = { original with 
                label = new_label cfg
              }
  in
  let node = { node with 
               label = dest (* do some label swapping to avoid having to find usages *)
             ; succ = [child.label]
             ; control = Goto child.label
             }
  in
  let nodes = IMap.add child.label child cfg.nodes in
  let nodes = IMap.add node.label node nodes 
  in
  { cfg with nodes = nodes }


let spill r cfg = 
  (* must calculate def and use first, as spilling introduces more defs/uses *)
  let defs = find_defs r cfg in
  let uses = find_uses r cfg in
  (* very similar to class code *)
  let spill_store r cfg = 
    let addr = Offset (Reg RBP, -8*(cfg.stack + 1)) in
    let inst = Memory (Store (addr, Reg r)) in
    let cfg = {cfg with stack = cfg.stack + 1 } in (* update size needed on stack *)
    let node = new_node 0 inst Stop (* label/control gets reset so we pick 0/stop *)
    in
    fold_left (insert_after node) cfg defs
  in
  let spill_load r cfg =
    let addr = Offset (Reg RBP, -8*(cfg.stack)) in
    let inst = Memory (Load (r, addr)) in
    let node = new_node 0 inst Stop  (* label/control gets reset so we pick 0/stop *)
    in
    fold_left (insert_before node) cfg uses
  in
  spill_load r (spill_store r cfg)