open Ast
open Eval

let optc = ref false
let opte = ref false
let file = ref ""

let parse_args () =
  let f s =
    if String.equal s "-c" then optc := true else
    if String.equal s "-e" then opte := true else
    file := s
  in
  let _ = Array.iter f Sys.argv in
  if String.equal !file ""
  then (prerr_string "Usage: c2 [-c] [-e] filename\n"; exit 1)

let parse_file () =
  let src =
    if !opte then Lexing.from_string !file
    else Lexing.from_channel (open_in !file)
  in
  Parse.program Lex.lexer src

let compile prog =
  let r = Compile.compile prog in
  print_endline (Compile.result2string r)

let eval prog =
  let ans = eval prog in
  print_endline ("answer = "^(Int64.to_string ans))

let main () =
  parse_args();
  let prog = parse_file() in
  Typecheck.typecheck prog;
  if !optc then compile prog else eval prog

let _ =
  try main ()
  with Typecheck.TypeError s -> print_endline ("type error:"^ s)

