type var = string

type typ =
  Long                                (* 64-bit integer type *)

type binop =
  Plus | Minus | Times | Div          (* +, -, *, /           *)
| Eq | Neq | Lt | Lte | Gt | Gte      (* ==, !=, <, <=, >, >= *)

type exp =
  Int of int64                        (* 12 *)
| Var of var                          (* x *)
| Binop of exp * binop * exp          (* x + y *)
| Not of exp                          (* !x *)
| And of exp * exp                    (* x && y *)
| Or of exp * exp                     (* x || y *)
| Assign of var * exp                 (* x = 3 *)
| Call of var * exp list              (* f(1,2,3) *)

type stmt =
  Exp of exp                          (* x = 3+4; *)
| Decl of typ * var                   (* int x; *)
| Block of stmt list                  (* { x = 2; ... } *)
| If of exp * stmt * stmt             (* if (x == y) x = 42; else y = 43; *)
| While of exp * stmt                 (* while (x < y) x = x + 1; *)
| For of exp * exp * exp * stmt       (* for (x=0; x<y; x=x+1) y=y*2; *)
| Return of exp                       (* return x; *)

let nop = Block []                    (* a "no operation" statement *)

type func =
  Fun of { rtyp: typ
         ; name: string
         ; args: (typ * string) list
         ; body: stmt list option
         }

type program = func list

let rec typ_str t =
  match t with
  | Long -> "long"

(* some functions to print out the AST in ocaml syntax *)
module ToStr = struct
open List

let rec typ_dbgstr t =
  match t with
  | Long -> "Long"

let op_dbgstr o =
  match o with
  | Plus  -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | Div   -> "Div"
  | Eq    -> "Eq"
  | Neq   -> "Neq"
  | Lt    -> "Lt"
  | Lte   -> "Lte"
  | Gt    -> "Gt"
  | Gte   -> "Gte"

let rec join sep l =
  match l with
  | []   -> ""
  | [x]  -> x
  | x::l -> x ^ sep ^ join sep l

let tuple s l = s ^ "("^ join "," l ^")"

let var v = "\""^ v ^ "\""

let rec exp_dbgstr e =
  match e with
  | Int i -> "Int "^ Int64.to_string i ^"L"
  | Var v -> tuple "Var" [var v]
  | Binop (e1,op,e2) ->
     tuple "Binop" [exp_dbgstr e1; op_dbgstr op; exp_dbgstr e2]
  | Not e -> etuple "Not" [e]
  | And (e1,e2) -> etuple "And" [e1;e2]
  | Or (e1,e2) -> etuple "Or" [e1;e2]
  | Assign (v,e2) -> tuple "Assign" [var v; exp_dbgstr e2]
  | Call (v,l) -> tuple "Call" (var v :: map exp_dbgstr l)

and etuple s l = tuple s (map exp_dbgstr l)

and stmt_dbgstr s =
  match s with
  | Exp e -> etuple "Exp" [e]
  | Decl(t,v) -> tuple "Decl" [typ_dbgstr t; var v]
  | Block l -> "Block"^ ast_dbgstr l
  | If (e,s1,s2) -> tuple "If" [exp_dbgstr e; stmt_dbgstr s1; stmt_dbgstr s2]
  | While (e,s) -> tuple "While" [exp_dbgstr e; stmt_dbgstr s]
  | For (e1,e2,e3,s) ->
     tuple "For" [exp_dbgstr e1; exp_dbgstr e2; exp_dbgstr e3; stmt_dbgstr s]
  | Return e -> etuple "Return" [e]

and ast_dbgstr l =
 "[" ^ join ";" (map stmt_dbgstr l) ^"]"

end

let typ_dbgstr = ToStr.typ_dbgstr
let exp_dbgstr = ToStr.exp_dbgstr
let stmt_dbgstr = ToStr.stmt_dbgstr
let ast_dbgstr = ToStr.ast_dbgstr

