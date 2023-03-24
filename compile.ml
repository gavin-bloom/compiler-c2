open List
open Ast
open X86
open Cfg

let rec drop n l =
  if n = 0 then l
  else match l with
  | [] -> []
  | _::l -> drop (n-1) l

module Env = Map.Make (
  struct
    type t = string
    let compare = String.compare
 end)

(* each function has it's own environment *)

type reg_slot = R of reg | S of int
type env = {
  label_counter: int ref; (* local labels *)
  var_counter: int ref;   (* local variables *)
  vars: reg_slot Env.t;   (* variable names to register/slots *)
}

let exit_label = 0

let new_label env = incr env.label_counter; !(env.label_counter)
let new_temp  env = incr env.var_counter; Var !(env.var_counter)
let set_var env v r = { env with vars = Env.add v r env.vars }
let add_var env v = set_var env v (R (new_temp env))
let get_var env v = Env.find v env.vars

let first_label (l: node list) = (hd l).label

let rec cfg_of_list env label k l : node list =
  match l with
  | []   -> raise (Failure "empty list in cfg_of_list")
  | [x]  -> [cfg_node label k x]
  | x::l -> let l = cfg_of_list env (new_label env) k l in
            cfg_node label (first_label l) x :: l

let rec binop_ b r1 r2 r3 =
  let bitop cc = [ cmp r1 r2; set cc r3; andi r3 1L ] in
  match b with
  | Plus  -> [ move r3 r1; Arith (Add (r3,Reg r2)) ]
  | Minus -> [ move r3 r1; Arith (Sub (r3,Reg r2)) ]
  | Times -> [ move RAX r1; Arith (IMul r2); move r3 RAX ]
  | Div   -> [ move RAX r1; Arith Cqo; Arith (IDiv r2); move r3 RAX ]
  | Eq    -> bitop E
  | Neq   -> bitop NE
  | Lt    -> bitop L
  | Lte   -> bitop LE
  | Gt    -> bitop G
  | Gte   -> bitop GE

let binop env l k b r1 r2 r3 =
    cfg_of_list env l k (binop_ b r1 r2 r3)

(* load from stack *)
let loads r i =
  let a = Offset (Reg RBP, -8*i) in
  Memory (Load (r, a))

(* store to stack *)
let stores i v =
  let a = Offset (Reg RBP, -8*i) in
  Memory (Store (a, v))

(* return bp offset and instructions *)
let rec exp env k e : reg * node list =
  match e with
  | Int i ->
      let l = new_label env in
      let t = new_temp env in
      (t, [cfg_node l k (movev t (Const i))])
  | Var v ->
      let l = new_label env in
      let t = new_temp env in
      begin match get_var env v with
      | R r -> (t, [ cfg_node l k (move t r) ])
      | S n -> (t, [ cfg_node l k (loads t n)])
      end
  | Binop (e1,b,e2) ->
     let l = new_label env in
     let (v2,l2) = exp env l e2 in
     let (v1,l1) = exp env (first_label l2) e1 in
     let v3 = new_temp env in
     (v3, l1 @ l2 @ binop env l k b v1 v2 v3)
  | Not e ->
     let l = new_label env in
     let (v1, l1) = exp env l e in
     let v2 = new_temp env in
     (v2, l1 @ cfg_of_list env l k
         [ cmpi v1 0L
         ; set E v2
         ; andi v2 1L ])
  | And (e1,e2) -> andor env k E  e1 e2
  | Or  (e1,e2) -> andor env k NE e1 e2
  | Assign (v,e) ->
     let l = new_label env in
     let (v1,l1) = exp env l e in
     begin match get_var env v with
     | R r -> (r,  l1 @ [ cfg_node l k (move r v1)])
     | S n -> (v1, l1 @ [ cfg_node l k (stores n (Reg v1))])
     end
  | Call(f,es) ->
     let arity = length es in
     let cl = new_label env in
     let v1 = new_temp env in
     let arg n v =
       match n with
       | 0 -> move RDI v
       | 1 -> move RSI v
       | 2 -> move RDX v
       | 3 -> move RCX v
       | 4 -> move R8 v
       | 5 -> move R9 v
       | _ -> push v
     in
     let rec args k n es =
       match es with
       | [] -> [ cfg_node (new_label env) k Nop ]
       | e::es ->
           let l = new_label env in
           let ls = args k (n+1) es in
           let (v1,l1) = exp env l e in
           l1 @ [ cfg_node l (first_label ls) (arg n v1) ] @ ls
     in
     let ax = args cl 0 (rev es) in
     let save = drop (min 6 arity) [RDI;RSI;RDX;RCX;R8;R9;R10;R11] in
     (v1, ax @ cfg_of_list env cl k
      (map push save @
       [ Call (arity, Label f) ] @
       map pop (rev save) @
       [ move v1 RAX ]))

and andor env k cc e1 e2 =
  let la = new_label env in
  let lb = new_label env in
  let skip = new_label env in
  let (v1,l1) = exp env la e1 in
  let (v2,l2) = exp env lb e2 in
  let v3 = new_temp env in
  (v3, l1 @
      [ new_node la (cmpi v1 0L) (Cond (cc,skip,first_label l2)) ]
      @ l2 @
      [ cfg_node lb skip (cmpi v2 0L) ]
      @ cfg_of_list env skip k
      [ set NE v3
      ; andi v3 1L ])

and addr env k e1 e2 =
  let l = new_label env in
  let (n,l2) = exp env l e2 in
  let (p,l1) = exp env (first_label l2) e1 in
  let t = new_temp env in
  (t, l1 @ l2 @ cfg_of_list env l k
  [ movei RAX 8L
  ; Arith(Mul n)
  ; Arith(Add (RAX, Reg p))
  ; move t RAX
  ])

let rec stmt env l k s : node list =
  match s with
  | Exp e ->
     let (v,l1) = exp env k e in
     cfg_node l (first_label l1) Nop :: l1
  | Decl (_,v) ->
     begin match get_var env v with
     | S _ -> raise (Failure "internal error")
     | R r -> [ cfg_node l k (movei r 0L) ]
     end
  | Block [] -> [ cfg_node l k Nop ]
  | Block ss -> stmts env l k ss
  | If (e,s1,s2) ->
     let l_test = new_label env in
     let (v,l1) = exp env l_test e in
     let l_true  = new_label env in
     let l_false = new_label env in
     let l2 = stmt env l_true  k s1 in
     let l3 = stmt env l_false k s2 in
     (cfg_node l (first_label l1) Nop :: l1 @
     [ new_node l_test (cmpi v 0L) (Cond (NE,l_true,l_false)) ]
     @ l2 @ l3)
  | While (e,s) ->
     let l_test = new_label env in
     let (v,l1) = exp env l_test e in
     let l_body = new_label env in
     let l2 = stmt env l_body l s in
     (cfg_node l (first_label l1) Nop :: l1 @
      [ new_node l_test (cmpi v 0L) (Cond (E,k,l_body)) ]
      @ l2)
  | For (e1,e2,e3,s) ->
     stmt env l k (Block [Exp e1; While (e2, Block [s; Exp e3])])
  | Return e ->
     let l_ret = new_label env in
     let (v,l1) = exp env l_ret e in
     cfg_node l (first_label l1) Nop ::
     l1 @ [ new_node l_ret (move RAX v) (Goto exit_label)]

and stmts env l k ss =
  match ss with
  | []    -> []
  | [s]   -> stmt env l k s
  | s::ss ->
     let env = match s with
               | Decl (_,v) -> add_var env v
               | _ -> env
     in
     let next = new_label env in
     stmt env l next s @ stmts env next k ss

let arg env n (_,name)  =
  let reg r   = set_var env name (R r) in
  let stack n = set_var env name (S n) in
  match n with
  | 0 -> reg RDI
  | 1 -> reg RSI
  | 2 -> reg RDX
  | 3 -> reg RCX
  | 4 -> reg R8
  | 5 -> reg R9
  | _ -> stack n

let rec args env n l =
  match l with
  | []   -> env
  | a::l -> args (arg env n a) (n+1) l

let func (Fun f) =
  let env = {
      label_counter = ref 0;
      var_counter = ref 0;
      vars=Env.empty }
  in
  let env = args env 0 f.args in
  let l_body = new_label env in
  let body = stmts env l_body exit_label (Option.get f.body) in
  let start = cfg_node (new_label env) l_body Nop in
  let start = { start with
                def = RegSet.of_list (first (length f.args) arguments) }
  in
  start :: body @
  [ new_node exit_label Nop Stop ]

let mk_cfg name f : cfg =
  let ins m n = IMap.add n.label n m in
  let ns = func f in
  { name  = name
  ; stack = 0
  ; start = first_label ns
  ; nodes = fold_left ins IMap.empty ns
  }

let rec funcs es fs l =
  match l with
  | []   -> (es,fs)
  | f::l ->
    match f with
    | Fun {name;body=None} -> funcs (name::es) fs l
    | Fun f ->
        let (f,name) = if f.name = "main"
                       then (Fun {f with name="main2"}, "main2")
                       else (Fun f, f.name)
        in funcs es (mk_cfg name f::fs) l

(* compilation driver *)

type result = {
  extern: string list;
  funcs: cfg list
}

let compile (p : Ast.program) : result =
  let (es,fs) = funcs [] [] p in
  (* iter print_cfg fs; *)
  let fs = map Regalloc.regalloc fs in
  { extern = es ; funcs = fs }

let catmap f l = String.concat "\n" (List.map f l)

let func_str cfg =
  let global s = "global "^ s ^"\n"^s^":\n" in
  global cfg.name ^
  global ("_"^ cfg.name) ^
  "push rbp\n" ^
  "mov rbp, rsp\n" ^
  "sub rsp,"^ string_of_int (cfg.stack*8) ^"\n" ^
  "push rbx\n"^
  "push r12\n"^
  "push r13\n"^
  "push r14\n"^
  "push r15\n"^
  catmap inst_str (cfg_to_x86 cfg) ^"\n"^
  ".exit:\n"^
  "pop r15\n"^
  "pop r14\n"^
  "pop r13\n"^
  "pop r12\n"^
  "pop rbx\n"^
  "mov rsp, rbp\n" ^
  "pop rbp\n" ^
  "ret\n"

let result2string (r : result) : string =
  "bits 64\n" ^
  "section .text\n" ^
  catmap (fun s -> "extern "^ s ^"\n") r.extern ^
  catmap func_str r.funcs
