open Ast

exception TypeError of string
let err s = raise (TypeError s)

module Env = Map.Make(
  struct
    type t = Ast.var
    let compare = String.compare
  end)

type env = func Env.t * typ Env.t

let empty_env : env = (Env.empty, Env.empty)

let add_func (fs,vs) (Fun f) =
  if Env.mem f.name fs
  then err ("duplicate function "^ f.name)
  else (Env.add f.name (Fun f) fs, vs)

let find_func (fs,_) n =
  match Env.find_opt n fs with
  | None -> err ("unknown function "^ n)
  | Some f -> f

let extend (fs,vs) v t = (fs,Env.add v t vs)
let find (_,vs) v =
  match Env.find_opt v vs with
  | None -> err ("unbound variable "^ v)
  | Some t -> t

let check_ty want have =
  if not (want = have) then
  err ("expecting "^ typ_str want ^" but found "^ typ_str have)

let rec tc_exp env e : typ =
  let tce e  = tc_exp env e in
  let long e = check_ty Long (tce e) in
  match e with
  | Int _ -> Long
  | Var v -> find env v
  | Binop (e1,_,e2) ->
     long e1; long e2; Long
  | Not e -> ignore(tce e); Long
  | And (e1,e2)
  | Or (e1,e2) ->
     ignore(tce e1);
     ignore(tce e2);
     Long
  | Assign (v,e) ->
     let t1 = find env v in
     let t2 = tce e in
     check_ty t1 t2;
     t1
  | Call (n,l) ->
     let Fun f = find_func env n in
     if List.length f.args != List.length l
     then err ("wrong number of arguments to "^ n);
     let chk_arg (t,_) e = check_ty t (tce e) in
     List.iter2 chk_arg f.args l;
     f.rtyp

let rec tc_stmts env l =
  match l with
  | [] -> ()
  | Decl(t,v)::ss -> tc_stmts (extend env v t) ss
  | s::ss ->
    let tcl l = tc_stmts env l in
    let tce e = ignore(tc_exp env e) in
    let rec tcs s =
      match s with
      | Exp e -> tce e
      | Decl (t,v) -> err "internal error"
      | Block l -> tcl l
      | If (e,s1,s2) -> tce e;tcs s1;tcs s2
      | While (e,s) -> tce e;tcs s
      | For (e1,e2,e3,s) -> tce e1;tce e2;tce e3;tcs s
      | Return e -> tce e
    in tcs s; tcl ss

let tc_func env (Fun f) =
  let arg env (t,v) = extend env v t in
  let env = List.fold_left arg env f.args in
  tc_stmts env (Option.get f.body)

let rec tc_funcs env fs =
  match fs with
  | [] ->
     if not (Env.mem "main" (fst env))
     then err "main is not defined"
  | f::fs ->
     let env = add_func env f in
     (match f with
     | Fun {body=Some _} -> tc_func env f
     | _ -> ());
     tc_funcs env fs

let typecheck (p : program) : unit =
  tc_funcs empty_env p
