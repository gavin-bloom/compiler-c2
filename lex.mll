{
open Parse
open Lexing
}

let eol = '\r'?'\n'
let ws  = ('\t'|' ')

let digit  = ['0'-'9']
let letter = ['A'-'Z''a'-'z']

let number = digit+
let ident  = (letter|'_')(letter|'_'|digit)*

(* rules section *)

rule lexer = parse
  | eol      { new_line lexbuf; lexer lexbuf }
  | ws+      { lexer lexbuf }
  | "/*"     { comment lexbuf }
  | eof      { EOF }
  | ';'      { SEMI }
  | ','      { COMMA }
  | '{'      { LBRACE }
  | '}'      { RBRACE }
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | '['      { LBRACK }
  | ']'      { RBRACK }
  | "=="     { EQEQ }
  | "="      { EQ }
  | "!="     { NEQ }
  | "<="     { LTE }
  | ">="     { GTE }
  | '<'      { LT }
  | '>'      { GT }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { TIMES }
  | '/'      { DIV }
  | "&&"     { AND }
  | "||"     { OR }
  | '!'      { BANG }
  | '&'      { REF }
  | "return" { RETURN }
  | "if"     { IF }
  | "else"   { ELSE }
  | "while"  { WHILE }
  | "for"    { FOR }
  | "long"   { LONG }
  | ident    { ID(Lexing.lexeme lexbuf) }
  | number   { INT(Int64.of_string(Lexing.lexeme lexbuf)) }

and comment = parse
  | eol      { new_line lexbuf; comment lexbuf }
  | "*/"     { lexer lexbuf }
  | _        { comment lexbuf }
