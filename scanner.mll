{ open Parser }
(* authored by: Bailey Hwa, help by Shida Jing *)
(* Citation: MicroC scanner *)


(*digits*)
let digit = ['0'-'9']
let digits = digit+

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf } (* Whitespace/filler*)
(* ---------- COMMENTS ----------- *)
| "/*"     { comment lexbuf }           (* comments *)
(*  Basic syntax  *)
| '('	{PAREN_L}
| ')'	{PAREN_R}
| '{'	{CURLY_L}
| '}'	{CURLY_R}
| '[' 	{SQUARE_L}
| ']'	{SQUARE_R}
| ';'	{SEMICOL}
| ','	{COMMA}
(*  Operators, both unary and binary  *)
| '+'	{ADD}
| '-' 	{SUB}
| '*'	{TIMES}
| '/'	{DIV}
| '='	{ASSIGN }


| "!"	{NOT}
| "=="	{EQ}
| "!="	{NEQ}
| '>'	{GT}
| '<'	{LT}
| "<="	{LEQ}
| ">="	{GEQ}
| '.'	{PERIOD}

(* Program flow  *)
| "&&" {AND}
| "||" {OR}
| "if"     {IF}
| "else"   {ELSE}
| "for"    {FOR}
| "while" {WHILE}
| "return" {RETURN}
| "new" {NEW}
| "del" {DEL}
| "NULL" {NULL}


(* Primitive data & function types *)
| "num"    {NUM}
| "bool"   {BOOL}
| "string" {STRING}
| "void"   {VOID}
| "xirtam"   {XIRTAM}



(*  Literals*)
| "true"   {TRUE}
| "false"  {FALSE}
| digits as lex { NUMLIT(float_of_string lex) } (*convert all numbers to float (num datatype)*)
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lex {NUMLIT(float_of_string lex) } (*accept floating point numbers with signs*)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lex {ID(lex)} (*Variable IDS string and number _*)
| '"' ([^ '"']* as lex) '"' { STRLIT(lex) } (*double quotes with lookahead*)
(*  Xirtam module functions*)
| eof { EOF }
| _ as char { raise (Failure("invalid character detectred: " ^ Char.escaped char)) }(* raise error *)

and comment = parse
 "*/" { tokenize lexbuf }
 | _ { comment lexbuf }