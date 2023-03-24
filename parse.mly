%{
open Ast
open Lexing

let value default opt =
  match opt with
  | None   -> Int default
  | Some x -> x

let parse_error s =
  let pos  = Parsing.symbol_end_pos () in
  let line = string_of_int (pos.pos_lnum) in
  print_endline ("line "^line^": "^s)
%}

%token EOF
%token SEMI COMMA LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK
%token EQEQ EQ NEQ LTE GTE LT GT
%token PLUS MINUS TIMES DIV
%token AND OR BANG
%token RETURN IF ELSE WHILE FOR
%token LONG REF
%token <string> ID
%token <int64> INT

%start program
%type <Ast.program> program

/* associativity, lowest precedence first */
%nonassoc THEN
%nonassoc ELSE

%left AND OR
%right EQ
%left EQEQ NEQ LT GT LTE GTE
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS BANG
%nonassoc LBRACK

/* Start of grammer rules */

%%

program:
  funclist EOF { $1 }

funclist:
| func { [$1] }
| func funclist { $1::$2 }

func:
| ty ID LPAREN args RPAREN SEMI
   { Fun {rtyp=$1;name=$2;args=$4;body=None}}
| ty ID LPAREN args RPAREN LBRACE stmtlist RBRACE
   { Fun {rtyp=$1;name=$2;args=$4;body=Some($7)}}

args:
|     { [] }
| arg { [$1] }
| arg COMMA args { $1::$3 }

arg:
| ty ID { ($1,$2) }

stmtlist :
| stmt { [$1] }
| stmt stmtlist { $1::$2 }

stmt :
| SEMI                                 { nop }
| exp SEMI                             { Exp $1 }
| ty ID SEMI                           { Decl ($1,$2) }
| RETURN exp SEMI                      { Return $2 }
| LBRACE stmtlist RBRACE               { Block $2 }
| IF LPAREN exp RPAREN stmt ELSE stmt  { If($3,$5,$7) }
| IF LPAREN exp RPAREN stmt %prec THEN { If($3,$5,nop) }
| WHILE LPAREN exp RPAREN stmt         { While($3,$5) }
| FOR LPAREN expopt SEMI expopt SEMI expopt RPAREN stmt {
    For(value 0L $3, value 1L $5, value 0L $7, $9)
  }

ty :
| LONG      { Long }

expopt :
|     { None }
| exp { Some $1 }

exp:
| INT                      { Int $1 }
| ID                       { Var $1 }
| LPAREN exp RPAREN        { $2 }
| MINUS exp %prec UMINUS   { Binop(Int 0L,Minus,$2) }
| exp PLUS  exp            { Binop($1,Plus,$3) }
| exp MINUS exp            { Binop($1,Minus,$3) }
| exp TIMES exp            { Binop($1,Times,$3) }
| exp DIV   exp            { Binop($1,Div,$3) }
| exp EQEQ  exp            { Binop($1,Eq,$3) }
| exp NEQ   exp            { Binop($1,Neq,$3) }
| exp LT    exp            { Binop($1,Lt,$3) }
| exp GT    exp            { Binop($1,Gt,$3) }
| exp LTE   exp            { Binop($1,Lte,$3) }
| exp GTE   exp            { Binop($1,Gte,$3) }
| BANG exp                 { Not $2 }
| exp AND   exp            { And($1,$3) }
| exp OR    exp            { Or($1,$3) }
| ID EQ exp                { Assign($1,$3) }
| ID LPAREN params RPAREN      { Call($1,$3) }

params:
|     { [] }
| exp { [$1] }
| exp COMMA params { $1::$3 }
