open Ast

(* Evaluate a statement.  We signal "returning" a value
 * for the program by throwing the exception Done.  *)
exception Done of int64
exception BadProgram

(* memory is an array of long values *)

let heap_size = 4096
let memory = Array.make heap_size 0L
let brk = ref 0

let alloc () =
  let n = !brk in
  if heap_size <= n
  then raise (Failure "out of memory");
  incr brk; Int64.of_int n

let load  n   = Array.get memory (Int64.to_int n)
let store n v = Array.set memory (Int64.to_int n) v

(* variables are addresses in memory *)

module Env = Map.Make(
  struct
    type t = string
    let compare = String.compare
  end)

let add_decl env v = Env.add v (alloc()) env
let address env v = Env.find v env
let value env v = load (address env v)
let set env v x = store (address env v) x; x

(* there is a set list of functions *)

let funs : (var * func) list ref = ref []
let get_func (n:var) : func =
  List.assoc n !funs

let bool2int b = if b then 1L else 0L

(* Evaluate an expression returning an integer *)
let rec eval_exp env e : int64 =
  let eval e = eval_exp env e in
  match e with
    Int i -> i
  | Var x -> value env x
  | Binop(e1,b,e2) ->
      let i1 = eval e1 in
      let i2 = eval e2 in
      (match b with
       | Plus  -> Int64.add i1 i2
       | Minus -> Int64.sub i1 i2
       | Times -> Int64.mul i1 i2
       | Div   -> Int64.div i1 i2
       | Eq    -> bool2int (i1 =  i2)
       | Neq   -> bool2int (i1 <> i2)
       | Lt    -> bool2int (i1 <  i2)
       | Lte   -> bool2int (i1 <= i2)
       | Gt    -> bool2int (i1 >  i2)
       | Gte   -> bool2int (i1 >= i2))
  | Not e1 -> bool2int (eval e1 = 0L)
  | And(e1,e2) ->
      if eval e1 <> 0L then
      if eval e2 <> 0L then 1L else 0L
      else 0L
  | Or(e1,e2) ->
      if eval e1 <> 0L then 1L else
      if eval e2 <> 0L then 1L else 0L
  | Assign(x,e1) -> set env x (eval e1)
  | Call (n,es) ->
      let es = List.map eval es in
      eval_fun n es

and eval_stmt env (s:stmt) : unit =
  let exp e = eval_exp env e in
  let stmt s = eval_stmt env s in
  match s with
  | Exp e -> ignore (exp e)
  | Decl (t,v) -> raise (Failure "internal error")
  | Block l -> eval_stmts env l
  | If(e,s1,s2) ->
      if exp e <> 0L then stmt s1 else stmt s2
  | While(e,s1) -> stmt (If(e, Block[s1;s], nop))
  | For(e1,e2,e3,s1) ->
      ignore (exp e1);
      stmt (While(e2, Block[s1;Exp e3]))
  | Return e -> raise (Done (exp e))

and eval_stmts env ss =
  match ss with
  | [] -> ()
  | Decl(_,v)::ss -> eval_stmts (add_decl env v) ss
  | s::ss -> eval_stmt env s; eval_stmts env ss

(* this only works for defined functions *)
and eval_fun n xs =
  let (Fun f) = get_func n in
  let vars = List.map snd f.args in
  let env = List.fold_left add_decl Env.empty vars in
  List.iter2 (fun v x -> ignore (set env v x)) vars xs;
  try
    eval_stmts env (Option.get f.body);
    print_string "Error -- function terminated without returning!\n";
    raise BadProgram
  with Done i -> i

let eval p =
  funs := List.map (fun (Fun f) -> (f.name, Fun f)) p;
  eval_fun "main" []
