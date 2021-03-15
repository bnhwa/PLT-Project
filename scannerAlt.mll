{ open Parser }
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
| '%'	{MOD}
| '^'	{EXP}
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
| "else if" {ELSEIF}
| "for"    {FOR}
| "return" {RETURN}
| "continue" { CONTINUE }
| "new" {NEW}
| "del" {DEL}
| "NULL" {NULL}

(*Note ARE WE IMPLEMENTING THESE?*)
(*| "break"  {BREAK}*)


(* Primitive data & function types *)
| "Xirtam" {XIRTAM}
| "num"    {NUM}
| "bool"   {BOOL}
| "string" {STRING}
| "function" {FUNC}
| "void"   {VOID}

(*  Literals*)
| "true"   {TRUE}
| "false"  {FALSE}
| ['+' '-']? digits as lex { NUMLIT(float_of_string lex) } (*convert all numbers to float (num datatype)*)
| ['+' '-']? digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lex {NUMLIT(float_of_string lex) } (*accept floating point numbers with signs*)
| ['_']?['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lex {ID(lex)}(*Variable IDS string and number _*)
| '"' ([^ '"']* as lex) '"' { STRLIT(lex) }(*double quotes with lookahead*)
| eof { EOF }
| _ as char { raise (Failure("invalid character detectred: " ^ Char.escaped char)) }(* raise error *)
