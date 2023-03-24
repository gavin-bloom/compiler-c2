
type label = int
type var = int
type reg =
  | RAX | RBX | RCX | RDX | RDI | RSI | RBP | RSP
  | R8  | R9  | R10 | R11 | R12 | R13 | R14 | R15
  | Var of var

type value =
  | Const of int64
  | Label of string
  | Reg of reg

type addr =
  | Addr of value
  | Offset of value * int

type memory =
  | Move of reg * value
  | Load of reg * addr
  | Store of addr * value
  | Push of value
  | Pop of reg
  | Pushf

type bitop =
  | Not of reg
  | And of reg * value
  | Or  of reg * value
  | Xor of reg * value
  | Ror of reg * int
  | Rol of reg * int
  | Shl of reg * int
  | Shr of reg * int
  | Sar of reg * int
  | Test of reg * value

type cc = E | NE | Z | NZ | L | LE | G | GE

type arith =
  | Inc of reg
  | Dec of reg
  | Neg of reg
  | Add of reg * value
  | Sub of reg * value
  | Mul of reg
  | Div of reg
  | Cqo
  | IMul of reg
  | IDiv of reg
  | Cmp of reg * value
  | Set of cc * reg

type cfg_inst =
  | Call of int * value
  | Ret
  | Nop
  | Memory of memory
  | Bitop of bitop
  | Arith of arith

type cfg_control =
    Stop
  | Goto of label
  | Cond of cc * label * label

type control =
  | Jump of value
  | Jcc of cc * value

type inst =
  | Label of string
  | Cfg of cfg_inst
  | Control of control

(* a few convience functions for constructing instructions *)

let movev r v  = Memory (Move (r, v))
let move r1 r2 = movev r1 (Reg r2)
let movei r i  = movev r (Const i)
let load r v   = Memory (Load (r, Addr v))
let store r v  = Memory (Store (Addr (Reg r), v))
let push r     = Memory (Push (Reg r))
let pop r      = Memory (Pop r)

let andi r i = Bitop (And (r, Const i))
let ori  r i = Bitop (Or  (r, Const i))
let xori r i = Bitop (Xor (r, Const i))
let tsti r i = Bitop (Test(r, Const i))
let tst  r v = Bitop (Test(r, v))

let cmp  x y = Arith (Cmp (x, Reg y))
let cmpi r i = Arith (Cmp (r, Const i))
let set cc r = Arith (Set (cc, r))

let addi r i = Arith (Add (r, Const (Int64.of_int i)))

let je   s = Control (Jcc (E,  Label s))
let jne  s = Control (Jcc (NE, Label s))
let jmp  s = Control (Jump (Label s))

(* define a submodle to keep "open X86" relatively clean *)
module X86ToStr = struct

  let reg_str r =
    match r with
    | RAX -> "rax" | RBX -> "rbx" | RCX -> "rcx" | RDX -> "rdx"
    | RDI -> "rdi" | RSI -> "rsi" | RBP -> "rbp" | RSP -> "rsp"
    | R8  -> "r8"  | R9  -> "r9"  | R10 -> "r10" | R11 -> "r11"
    | R12 -> "r12" | R13 -> "r13" | R14 -> "r14" | R15 -> "r15"
    | Var i -> "v_" ^ string_of_int i

  let reg8_str r =
    match r with
    | RAX -> "al"   | RBX -> "bl"   | RCX -> "cl"   | RDX -> "dl"
    | RDI -> "dil"  | RSI -> "sil"  | RBP -> "bpl"  | RSP -> "spl"
    | R8  -> "r8b"  | R9  -> "r9b"  | R10 -> "r10b" | R11 -> "r11b"
    | R12 -> "r12b" | R13 -> "r13b" | R14 -> "r14b" | R15 -> "r15b"
    | Var i -> "v_" ^ string_of_int i

  let rec value_str v =
    match v with
    | Const i -> Int64.to_string i
    | Label s -> s
    | Reg r   -> reg_str r

  let addr_str a =
    match a with
    | Addr v -> "["^ value_str v ^"]"
    | Offset (v,i) -> "["^ value_str v ^ " + " ^ Int.to_string i ^"]"

  let memory_str m =
    match m with
    | Move  (r,v) -> "mov "^ reg_str r ^", "^ value_str v
    | Load  (r,a) -> "mov "^ reg_str r ^", "^ addr_str a
    | Store (a,v) -> "mov qword "^ addr_str a ^", "^ value_str v
    | Push r -> "push "^ value_str r
    | Pop  r -> "pop " ^ reg_str r
    | Pushf -> "pushf"

  let rr a b = reg_str a ^", "^ reg_str b
  let rv r v = reg_str r ^", "^ value_str v
  let ri r i = reg_str r ^", "^ string_of_int i

  let bitop_str b =
    match b with
    | Not r -> "not "^ reg_str r
    | And (a,b) -> "and "^ rv a b
    | Or  (a,b) -> "or " ^ rv a b
    | Xor (a,b) -> "xor "^ rv a b
    | Ror (a,b) -> "ror "^ ri a b
    | Rol (a,b) -> "rol "^ ri a b
    | Shl (a,b) -> "shl "^ ri a b
    | Shr (a,b) -> "shr "^ ri a b
    | Sar (a,b) -> "sar "^ ri a b
    | Test (a,b) -> "test "^ rv a b

  let cc_str c =
    match c with
    | E  -> "e"
    | NE -> "ne"
    | Z  -> "z"
    | NZ -> "nz"
    | L  -> "l"
    | LE -> "le"
    | G  -> "g"
    | GE -> "ge"

  let arith_str a =
    match a with
    | Inc a -> "inc "^ reg_str a
    | Dec a -> "dec "^ reg_str a
    | Neg a -> "neg "^ reg_str a
    | Add (a,b) -> "add "^ rv a b
    | Sub (a,b) -> "sub "^ rv a b
    | Mul a -> "mul "^ reg_str a
    | Div a -> "div "^ reg_str a
    | Cqo -> "cqo"
    | IMul a -> "imul "^ reg_str a
    | IDiv a -> "idiv "^ reg_str a
    | Cmp (a,b) -> "cmp "^ rv a b
    | Set (cc,r) -> "set"^cc_str cc^" "^ reg8_str r

  let cfg_str c =
    match c with
    | Call (_,v) -> "call "^ value_str v
    | Ret -> "ret"
    | Nop -> "nop"
    | Memory m -> memory_str m
    | Bitop b -> bitop_str b
    | Arith a -> arith_str a

  let cfg_control_str c =
    match c with
    | Stop -> "stop"
    | Goto l -> "goto "^ string_of_int l
    | Cond (cc,l1,l2) ->
      "cond "^ cc_str cc ^" "^
      string_of_int l1 ^" "^
      string_of_int l2

  let control_str c =
    match c with
    | Jump  v -> "jmp "^ value_str v
    | Jcc  (c, v) -> "j"^cc_str c ^" "^ value_str v

  let inst_str i =
    match i with
    | Label s -> s^":"
    | Cfg c -> cfg_str c
    | Control c -> control_str c

end

let reg_str = X86ToStr.reg_str
let cfg_str = X86ToStr.cfg_str
let cfg_control_str = X86ToStr.cfg_control_str
let inst_str = X86ToStr.inst_str

let rec window2 f insts =
  match insts with
  | a::b::xs ->
    (match f a b with
     | None   -> a :: window2 f (b::xs)
     | Some l -> window2 f (l @ xs))
  | _ -> insts


let rec window1 f insts =
  match insts with
  | a::xs ->
    (match f a with
     | None   -> window1 f xs
     | Some a -> a :: (window1 f xs))
  | [] -> []

(* register subsitution *)

let subst_reg s r =
  match List.assoc_opt r s with
  | None -> r
  | Some r -> r

let subst_value s v =
  match v with
  | Const i -> Const i
  | Label s -> Label s
  | Reg r -> Reg (subst_reg s r)

let subst_addr s a =
  match a with
  | Addr v -> Addr (subst_value s v)
  | Offset (v,i) -> Offset (subst_value s v, i)

let subst_memory s m =
  match m with
  | Move (r,v) -> Move (subst_reg s r, subst_value s v)
  | Load (r,a) -> Load (subst_reg s r, subst_addr s a)
  | Store (a,v) -> Store (subst_addr s a, subst_value s v)
  | Push v -> Push (subst_value s v)
  | Pop r -> Pop (subst_reg s r)
  | Pushf -> Pushf

let subst_bitop s b =
  match b with
  | Not r -> Not (subst_reg s r)
  | And (r,v) -> And (subst_reg s r, subst_value s v)
  | Or  (r,v) -> Or  (subst_reg s r, subst_value s v)
  | Xor (r,v) -> Xor (subst_reg s r, subst_value s v)
  | Ror (r,i) -> Ror (subst_reg s r, i)
  | Rol (r,i) -> Rol (subst_reg s r, i)
  | Shl (r,i) -> Shl (subst_reg s r, i)
  | Shr (r,i) -> Shr (subst_reg s r, i)
  | Sar (r,i) -> Sar (subst_reg s r, i)
  | Test(r,v) -> Test (subst_reg s r, subst_value s v)

let subst_arith s a =
  match a with
  | Inc r -> Inc (subst_reg s r)
  | Dec r -> Dec (subst_reg s r)
  | Neg r -> Neg (subst_reg s r)
  | Add (r,v) -> Add (subst_reg s r, subst_value s v)
  | Sub (r,v) -> Sub (subst_reg s r, subst_value s v)
  | Mul r -> Mul (subst_reg s r)
  | Div r -> Div (subst_reg s r)
  | Cqo -> Cqo
  | IMul r -> IMul (subst_reg s r)
  | IDiv r -> IDiv (subst_reg s r)
  | Cmp (r,v) -> Cmp (subst_reg s r, subst_value s v)
  | Set (cc,r) -> Set (cc, subst_reg s r)

let subst_cfg_inst s inst =
  match inst with
  | Call (i,v) -> Call (i, subst_value s v)
  | Ret -> Ret
  | Nop -> Nop
  | Memory m -> Memory (subst_memory s m)
  | Bitop b -> Bitop (subst_bitop s b)
  | Arith a -> Arith (subst_arith s a)

let subst_inst s inst =
  match inst with
  | Label s -> Label s
  | Cfg i -> Cfg (subst_cfg_inst s i)
  | Control c -> Control c

