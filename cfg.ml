open List
open X86

let first n = filteri (fun i -> fun _ -> i < n)

module RegSet = Set.Make(
  struct
    type t = X86.reg
    let compare = compare
  end)

let (++)      = RegSet.union
let (--)      = RegSet.diff
let empty     = RegSet.empty
let union_all = fold_left (++) empty

module IMap = Map.Make(
  struct
    type t = int
    let compare = Int.compare
  end)

let arguments =
  [ RDI; RSI; RDX; RCX; R8; R9 ]

let succ control =
  match control with
  | Stop -> []
  | Goto l -> [l]
  | Cond (_,l1,l2) -> [l1;l2]

let use cfg_inst =
  let value = function
    | Reg r -> [r]
    | _ -> []
  in
  let addr = function
    | Addr v | Offset (v,_) -> value v
  in
  let memory = function
    | Move (_,v) -> value v
    | Load (_,a) -> addr a
    | Store (a,v) -> addr a @ value v
    | Push v -> value v
    | _ -> []
  in
  let bitop = function
    | Not r | Ror(r,_) | Rol(r,_) | Shl(r,_) | Shr(r,_) | Sar(r,_) -> [r]
    | And(r,v) | Or(r,v) | Xor(r,v) | Test(r,v) -> r :: value v
  in
  let arith = function
    | Inc r | Dec r | Neg r -> [r]
    | Add(r,v) | Sub(r,v) | Cmp (r,v) -> r :: value v
    | Mul r | IMul r -> [RAX ; r]
    | Div r | IDiv r -> [RAX ; RDX ; r]
    | Cqo -> [RAX]
    | Set (_,r) -> []
  in
  let inst = function
    | Call (i,v) -> first i arguments @ value v
    | Ret        -> [ RAX ]
    | Nop        -> []
    | Memory m   -> memory m
    | Bitop b    -> bitop b
    | Arith a    -> arith a
  in
  RegSet.of_list (inst cfg_inst)

let def cfg_inst =
  let memory = function
    | Move (r,_) | Load (r,_) | Pop r -> [r]
    | _ -> []
  in
  let bitop = function
    | Not r | Ror(r,_) | Rol(r,_) | Shl(r,_) | Shr(r,_) | Sar(r,_)
    | And(r,_) | Or(r,_) | Xor(r,_) | Test(r,_) -> [r]
  in
  let arith = function
    | Inc r | Dec r | Neg r
    | Add(r,_) | Sub(r,_) | Cmp (r,_) -> [r]
    | Mul _ | IMul _
    | Div _ | IDiv _
    | Cqo -> [RAX ; RDX]
    | Set (_,r) -> [r]
  in
  let inst = function
    | Call _   -> [RAX]
    | Ret      -> []
    | Nop      -> []
    | Memory m -> memory m
    | Bitop b  -> bitop b
    | Arith a  -> arith a
  in
  RegSet.of_list (inst cfg_inst)

type node =
  { label: label           (* node label *)
  ; succ: label list       (* list of successors *)
  ; stmt: cfg_inst         (* instructon *)
  ; control: cfg_control   (* control flow *)
  ; def: RegSet.t          (* set of vars defined *)
  ; use: RegSet.t          (* set of vars used *)
  ; live_in: RegSet.t      (* live variables in *)
  ; live_out: RegSet.t     (* live variables out *)
  }

let new_node label stmt control =
  { label    = label
  ; succ     = succ control
  ; stmt     = stmt
  ; control  = control
  ; def      = def stmt
  ; use      = use stmt
  ; live_in  = RegSet.empty
  ; live_out = RegSet.empty
  }

let cfg_node l k s =
  match s with
  | Ret -> new_node l s Stop
  | _   -> new_node l s (Goto k)

let subst_node s n =
  let stmt = subst_cfg_inst s n.stmt in
  { n with stmt = stmt
         ; def = def stmt
         ; use = use stmt
         ; live_in = RegSet.empty
         ; live_out = RegSet.empty
  }

(* control-flow graph for a single function *)

type cfg =
  { name: string
  ; stack: int          (* number of stack slots needed *)
  ; start: label        (* entry node *)
  ; nodes: node IMap.t  (* map containing nodes by label *)
  }

let new_label cfg =
  (fst (IMap.max_binding cfg.nodes)) + 1

let add_node cfg b = { cfg with nodes = IMap.add b.label b cfg.nodes }

let print_node n =
  let s = string_of_int n.label ^":"^
          cfg_str n.stmt ^" ; "^
          cfg_control_str n.control
  in print_endline s

let print_cfg (cfg:cfg):unit =
  let node_list ns = map snd (IMap.bindings ns) in
  print_endline (cfg.name ^ " CFG");
  iter print_node (node_list cfg.nodes)

(* liveness *)

let visit_nodes (cfg:cfg) (f:node IMap.t -> node -> node) =
  let visited : label list ref = ref [] in
  let add n = visited := n :: !visited in
  let seen n = List.mem n !visited in

  let nodes = ref cfg.nodes in
  let node n = IMap.find n !nodes in

  let rec visit n =
    if not (seen n) then
      let b = f !nodes (node n) in
      (nodes := IMap.add n b !nodes;
       add n;
       List.iter visit b.succ)
  in
  (visit cfg.start; { cfg with nodes = !nodes })

let subst_cfg s cfg =
  visit_nodes cfg (fun _ n -> subst_node s n)

let rec liveness cfg =
  let changed = ref false in
  let visit nodes b =
    let node n    = IMap.find n nodes in
    let in_of n   = (node n).live_in in
    let live_in   = b.use ++ (b.live_out -- b.def) in
    let live_out  = union_all (map in_of b.succ) in
    changed := !changed ||
               not (live_in = b.live_in) ||
               not (live_out = b.live_out);
    { b with live_in = live_in; live_out = live_out }
  in
  let cfg = visit_nodes cfg visit in
  if !changed then liveness cfg else cfg

(* Conversion to X86 *)

let cleanup inst =
  let f x y = match (x,y) with
    | (Control (Jump (Label l1)), Label l2)
      when l1 = l2 -> Some [ Label l2 ]
    (*| (Cfg Nop,y)
      | (y,Cfg Nop) -> Some [y] *)
    | _ -> None
  in
  let inst = window2 f inst in
  (* remove self stores with a window 1 *)
  let f a =
    match a with 
    | Cfg (Memory (Move (r1, Reg (r2)))) when r1 = r2 -> None
    | _ -> Some a
  in
  let inst = window1 f inst in
  let f = function
    | Control (Jump (Label l))
    | Control (Jcc (_,Label l)) -> Some l
    | _ -> None
  in
  let used = filter_map f inst in
  let f = function
    | Label l when not (mem l used) -> None
    | x -> Some x
  in
  filter_map f inst

let map_nodes (cfg:cfg) (f: node -> 'a list) : 'a list =
  let visited : label list ref = ref [] in
  let add l = visited := l :: !visited in
  let seen l = List.mem l !visited in

  let node l = IMap.find l cfg.nodes in

  let rec visit l =
    if seen l then []
    else
      let n = node l in
      let a = f n in
      add l;
      a @ List.concat_map visit n.succ
  in
  visit cfg.start

let cfg_to_x86 cfg =
  let lstr l = ".l"^ string_of_int l in
  let lbl  l : value = Label (lstr l) in
  let f n =
    [ Label (lstr n.label)
    ; Cfg n.stmt
    ] @
    match n.control with
    | Stop -> [ Control (Jump (Label ".exit")) ]
    | Goto l -> [ Control (Jump (lbl l)) ]
    | Cond (cc,l1,l2) ->
      [ Control (Jcc (cc,lbl l1))
      ; Control (Jump (lbl l2)) ]
  in
  cleanup (map_nodes cfg f)
