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
| '='	{ASSIGN }
(*optional operators? should we implement these later, putting them here so we can just uncomment and go| '%' {MOD}*)
| '%' {MOD}
(* | '^'  {EXP} *)

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
(*| "continue" { CONTINUE }*)(*no more continue*)
| "new" {NEW}
| "del" {DEL}
| "NULL" {NULL}

(*Note ARE WE IMPLEMENTING THESE?*)
(*| "break"  {BREAK}*)


(* Primitive data & function types *)
| "num"    {NUM}
| "bool"   {BOOL}
| "string" {STRING}
| "void"   {VOID}
| "xirtam"   {XIRTAM}
(* Xirtam functions CHECK IF THIS CONFLICTS WITH VAR NAMING
if so, make it so users cant name variables xirtam function names
*)
(* MAT_IDENTITY MAT_FILL MAT_TRANSPOSE MAT_ROWS MAT_COLS MAT_EQ MAT_ADD MAT_MULT_SCALAR MAT_MULT*)

(*| "identity" {MAT_IDENTITY} *)

| "fillMat" {MAT_FILL}  (* Fill all of matrix with values*)
| "transpose" {MAT_TRANSPOSE}(* *)
| "getrows" {MAT_ROWS}(* get number of rows*)(* *)
| "getcols" {MAT_COLS}(* get number of cols*)
| "equals" {MAT_EQ} (* *)
| "addMat" {MAT_ADD}(* *)
| "multScalar" {MAT_MULT_SCALAR}(*multiply scalar *)
| "multMat" {MAT_MULT}(*mult 2 matrices *)


(*  Literals*)
| "true"   {TRUE}
| "false"  {FALSE}
| digits as lex { NUMLIT(float_of_string lex) } (*convert all numbers to float (num datatype)*)
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lex {NUMLIT(float_of_string lex) } (*accept floating point numbers with signs*)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lex {ID(lex)}(*Variable IDS string and number _*)
| '"' ([^ '"']* as lex) '"' { STRLIT(lex) }(*double quotes with lookahead*)
(*  Xirtam module functions*)
| eof { EOF }
| _ as char { raise (Failure("invalid character detectred: " ^ Char.escaped char)) }(* raise error *)

and comment = parse
 "*/" { tokenize lexbuf }
 | _ { comment lexbuf }