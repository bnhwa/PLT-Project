type token =
  | PAREN_L
  | PAREN_R
  | CURLY_L
  | CURLY_R
  | SQUARE_L
  | SQUARE_R
  | SEMICOL
  | COMMA
  | ADD
  | SUB
  | TIMES
  | ASSIGN
  | NOT
  | EQ
  | NEQ
  | GT
  | LT
  | LEQ
  | GEQ
  | PERIOD
  | TRUE
  | FALSE
  | DIV
  | MOD
  | AND
  | OR
  | IF
  | ELSE
  | FOR
  | WHILE
  | RETURN
  | NEW
  | DEL
  | NULL
  | MAT_FILL
  | MAT_TRANSPOSE
  | MAT_ROWS
  | MAT_COLS
  | MAT_EQ
  | MAT_ADD
  | MAT_MULT_SCALAR
  | MAT_MULT
  | NUM
  | BOOL
  | STRING
  | VOID
  | XIRTAM
  | NUMLIT of (float)
  | BOOLLIT of (bool)
  | ID of (string)
  | STRLIT of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 7 "parser.mly"
 open Ast 
# 60 "parser.ml"
let yytransl_const = [|
  257 (* PAREN_L *);
  258 (* PAREN_R *);
  259 (* CURLY_L *);
  260 (* CURLY_R *);
  261 (* SQUARE_L *);
  262 (* SQUARE_R *);
  263 (* SEMICOL *);
  264 (* COMMA *);
  265 (* ADD *);
  266 (* SUB *);
  267 (* TIMES *);
  268 (* ASSIGN *);
  269 (* NOT *);
  270 (* EQ *);
  271 (* NEQ *);
  272 (* GT *);
  273 (* LT *);
  274 (* LEQ *);
  275 (* GEQ *);
  276 (* PERIOD *);
  277 (* TRUE *);
  278 (* FALSE *);
  279 (* DIV *);
  280 (* MOD *);
  281 (* AND *);
  282 (* OR *);
  283 (* IF *);
  284 (* ELSE *);
  285 (* FOR *);
  286 (* WHILE *);
  287 (* RETURN *);
  288 (* NEW *);
  289 (* DEL *);
  290 (* NULL *);
  291 (* MAT_FILL *);
  292 (* MAT_TRANSPOSE *);
  293 (* MAT_ROWS *);
  294 (* MAT_COLS *);
  295 (* MAT_EQ *);
  296 (* MAT_ADD *);
  297 (* MAT_MULT_SCALAR *);
  298 (* MAT_MULT *);
  299 (* NUM *);
  300 (* BOOL *);
  301 (* STRING *);
  302 (* VOID *);
  303 (* XIRTAM *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  304 (* NUMLIT *);
  305 (* BOOLLIT *);
  306 (* ID *);
  307 (* STRLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\006\000\006\000\004\000\
\007\000\007\000\009\000\009\000\010\000\010\000\011\000\011\000\
\005\000\005\000\005\000\005\000\005\000\008\000\008\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\014\000\014\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\003\000\000\000\002\000\009\000\
\000\000\001\000\002\000\004\000\000\000\001\000\001\000\003\000\
\001\000\001\000\001\000\001\000\001\000\000\000\002\000\002\000\
\003\000\003\000\005\000\007\000\009\000\005\000\000\000\001\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\002\000\003\000\004\000\003\000\
\003\000\005\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\059\000\000\000\017\000\018\000\019\000\020\000\
\021\000\001\000\003\000\004\000\000\000\000\000\000\000\005\000\
\000\000\000\000\000\000\011\000\000\000\000\000\006\000\000\000\
\000\000\012\000\007\000\000\000\000\000\000\000\000\000\022\000\
\008\000\000\000\000\000\000\000\033\000\034\000\000\000\000\000\
\000\000\000\000\036\000\000\000\035\000\000\000\023\000\000\000\
\000\000\000\000\000\000\052\000\053\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\024\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\056\000\026\000\000\000\000\000\038\000\000\000\
\000\000\000\000\025\000\000\000\000\000\000\000\000\000\000\000\
\041\000\000\000\000\000\000\000\000\000\000\000\000\000\042\000\
\043\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\055\000\000\000\000\000\000\000\000\000\030\000\058\000\000\000\
\000\000\028\000\000\000\000\000\029\000"

let yydgoto = "\002\000\
\003\000\004\000\011\000\012\000\013\000\025\000\018\000\029\000\
\019\000\084\000\077\000\046\000\047\000\058\000\051\000"

let yysindex = "\012\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\222\254\043\255\036\000\000\000\
\226\254\028\255\053\255\000\000\048\255\036\000\000\000\027\255\
\036\000\000\000\000\000\031\255\044\255\045\255\005\255\000\000\
\000\000\079\255\005\255\005\255\000\000\000\000\097\255\099\255\
\100\255\005\255\000\000\002\255\000\000\167\000\000\000\215\255\
\075\255\005\255\102\255\000\000\000\000\005\255\005\255\005\255\
\205\000\105\255\005\255\005\255\000\000\005\255\005\255\005\255\
\005\255\005\255\005\255\005\255\005\255\005\255\005\255\005\255\
\005\255\005\255\000\000\000\000\056\255\205\000\000\000\233\255\
\108\255\251\255\000\000\111\255\109\255\205\000\035\255\035\255\
\000\000\251\000\251\000\254\254\254\254\254\254\254\254\000\000\
\000\000\240\000\223\000\113\255\005\255\150\255\005\255\150\255\
\000\000\079\255\205\000\094\255\187\000\000\000\000\000\150\255\
\005\255\000\000\122\255\150\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\127\255\000\000\
\000\000\000\000\128\255\000\000\000\000\000\000\000\000\000\000\
\106\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\124\255\000\000\197\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\124\255\000\000\
\010\255\000\000\130\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\112\255\000\000\000\000\
\000\000\000\000\000\000\000\000\132\255\021\000\024\000\049\000\
\000\000\168\255\154\000\070\000\091\000\112\000\133\000\000\000\
\000\000\061\255\184\255\133\255\000\000\000\000\000\000\000\000\
\000\000\000\000\164\000\137\255\000\000\000\000\000\000\000\000\
\141\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\119\000\000\000\068\000\000\000\000\000\113\000\
\000\000\000\000\087\000\225\255\243\255\203\255\042\000"

let yytablesize = 531
let yytable = "\048\000\
\010\000\081\000\059\000\052\000\053\000\031\000\062\000\063\000\
\064\000\034\000\057\000\032\000\001\000\060\000\035\000\014\000\
\032\000\036\000\078\000\020\000\071\000\072\000\080\000\057\000\
\082\000\037\000\038\000\078\000\086\000\021\000\087\000\088\000\
\089\000\090\000\091\000\092\000\093\000\094\000\095\000\096\000\
\097\000\098\000\099\000\015\000\031\000\064\000\032\000\033\000\
\034\000\016\000\023\000\016\000\043\000\035\000\044\000\045\000\
\036\000\071\000\072\000\115\000\022\000\100\000\050\000\101\000\
\037\000\038\000\050\000\050\000\050\000\107\000\039\000\109\000\
\040\000\041\000\042\000\031\000\026\000\032\000\076\000\034\000\
\030\000\057\000\017\000\050\000\035\000\050\000\050\000\036\000\
\108\000\024\000\110\000\043\000\028\000\044\000\045\000\037\000\
\038\000\054\000\114\000\055\000\056\000\039\000\117\000\040\000\
\041\000\042\000\022\000\079\000\022\000\022\000\022\000\083\000\
\105\000\015\000\103\000\022\000\101\000\015\000\022\000\015\000\
\106\000\112\000\043\000\116\000\044\000\045\000\022\000\022\000\
\009\000\010\000\031\000\013\000\022\000\014\000\022\000\022\000\
\022\000\027\000\057\000\027\000\027\000\027\000\031\000\027\000\
\049\000\085\000\027\000\111\000\000\000\027\000\031\000\000\000\
\032\000\022\000\034\000\022\000\022\000\027\000\027\000\035\000\
\000\000\000\000\036\000\027\000\000\000\027\000\027\000\027\000\
\000\000\044\000\037\000\038\000\000\000\044\000\044\000\044\000\
\039\000\000\000\040\000\041\000\042\000\044\000\044\000\000\000\
\027\000\051\000\027\000\027\000\000\000\051\000\051\000\051\000\
\044\000\044\000\000\000\000\000\000\000\043\000\037\000\044\000\
\045\000\000\000\037\000\037\000\037\000\037\000\037\000\037\000\
\000\000\051\000\037\000\037\000\037\000\037\000\037\000\037\000\
\075\000\000\000\000\000\037\000\037\000\037\000\037\000\062\000\
\063\000\064\000\000\000\000\000\065\000\066\000\067\000\068\000\
\069\000\070\000\102\000\000\000\000\000\071\000\072\000\073\000\
\074\000\062\000\063\000\064\000\000\000\000\000\065\000\066\000\
\067\000\068\000\069\000\070\000\104\000\000\000\000\000\071\000\
\072\000\073\000\074\000\062\000\063\000\064\000\000\000\000\000\
\065\000\066\000\067\000\068\000\069\000\070\000\000\000\000\000\
\000\000\071\000\072\000\073\000\074\000\000\000\054\000\000\000\
\000\000\039\000\054\000\054\000\054\000\039\000\039\000\039\000\
\039\000\039\000\000\000\000\000\000\000\039\000\039\000\039\000\
\039\000\039\000\039\000\005\000\006\000\007\000\008\000\009\000\
\039\000\039\000\040\000\000\000\000\000\000\000\040\000\040\000\
\040\000\040\000\040\000\000\000\000\000\000\000\040\000\040\000\
\040\000\040\000\040\000\040\000\000\000\000\000\000\000\046\000\
\000\000\040\000\040\000\046\000\046\000\046\000\005\000\006\000\
\007\000\008\000\009\000\046\000\046\000\046\000\046\000\046\000\
\046\000\000\000\000\000\000\000\047\000\000\000\046\000\046\000\
\047\000\047\000\047\000\000\000\000\000\000\000\000\000\000\000\
\047\000\047\000\047\000\047\000\047\000\047\000\000\000\000\000\
\000\000\049\000\000\000\047\000\047\000\049\000\049\000\049\000\
\000\000\000\000\000\000\000\000\000\000\049\000\049\000\049\000\
\049\000\049\000\049\000\000\000\000\000\000\000\048\000\000\000\
\049\000\049\000\048\000\048\000\048\000\000\000\000\000\000\000\
\000\000\000\000\048\000\048\000\048\000\048\000\048\000\048\000\
\000\000\000\000\000\000\045\000\000\000\048\000\048\000\045\000\
\045\000\045\000\000\000\000\000\000\000\016\000\000\000\045\000\
\045\000\016\000\000\000\016\000\000\000\061\000\000\000\062\000\
\063\000\064\000\045\000\045\000\065\000\066\000\067\000\068\000\
\069\000\070\000\000\000\000\000\000\000\071\000\072\000\073\000\
\074\000\113\000\000\000\062\000\063\000\064\000\000\000\000\000\
\065\000\066\000\067\000\068\000\069\000\070\000\000\000\000\000\
\000\000\071\000\072\000\073\000\074\000\062\000\063\000\064\000\
\000\000\000\000\065\000\066\000\067\000\068\000\069\000\070\000\
\000\000\000\000\000\000\071\000\072\000\073\000\074\000\062\000\
\063\000\064\000\000\000\000\000\065\000\066\000\067\000\068\000\
\069\000\070\000\000\000\000\000\000\000\071\000\072\000\073\000\
\062\000\063\000\064\000\000\000\000\000\065\000\066\000\067\000\
\068\000\069\000\070\000\062\000\063\000\064\000\071\000\072\000\
\000\000\000\000\067\000\068\000\069\000\070\000\000\000\000\000\
\000\000\071\000\072\000"

let yycheck = "\031\000\
\000\000\055\000\001\001\035\000\036\000\001\001\009\001\010\001\
\011\001\005\001\042\000\002\001\001\000\012\001\010\001\050\001\
\007\001\013\001\050\000\050\001\023\001\024\001\054\000\055\000\
\056\000\021\001\022\001\059\000\060\000\002\001\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\069\000\070\000\071\000\
\072\000\073\000\074\000\001\001\001\001\011\001\003\001\004\001\
\005\001\007\001\003\001\007\001\048\001\010\001\050\001\051\001\
\013\001\023\001\024\001\113\000\008\001\006\001\002\001\008\001\
\021\001\022\001\006\001\007\001\008\001\101\000\027\001\103\000\
\029\001\030\001\031\001\001\001\050\001\003\001\004\001\005\001\
\050\001\113\000\015\000\005\001\010\001\025\001\026\001\013\001\
\102\000\022\000\104\000\048\001\025\000\050\001\051\001\021\001\
\022\001\001\001\112\000\001\001\001\001\027\001\116\000\029\001\
\030\001\031\001\001\001\006\001\003\001\004\001\005\001\007\001\
\002\001\002\001\007\001\010\001\008\001\006\001\013\001\008\001\
\008\001\028\001\048\001\002\001\050\001\051\001\021\001\022\001\
\002\001\002\001\007\001\002\001\027\001\002\001\029\001\030\001\
\031\001\001\001\006\001\003\001\004\001\005\001\002\001\025\000\
\032\000\059\000\010\001\106\000\255\255\013\001\001\001\255\255\
\003\001\048\001\005\001\050\001\051\001\021\001\022\001\010\001\
\255\255\255\255\013\001\027\001\255\255\029\001\030\001\031\001\
\255\255\002\001\021\001\022\001\255\255\006\001\007\001\008\001\
\027\001\255\255\029\001\030\001\031\001\014\001\015\001\255\255\
\048\001\002\001\050\001\051\001\255\255\006\001\007\001\008\001\
\025\001\026\001\255\255\255\255\255\255\048\001\002\001\050\001\
\051\001\255\255\006\001\007\001\008\001\009\001\010\001\011\001\
\255\255\026\001\014\001\015\001\016\001\017\001\018\001\019\001\
\002\001\255\255\255\255\023\001\024\001\025\001\026\001\009\001\
\010\001\011\001\255\255\255\255\014\001\015\001\016\001\017\001\
\018\001\019\001\002\001\255\255\255\255\023\001\024\001\025\001\
\026\001\009\001\010\001\011\001\255\255\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\002\001\255\255\255\255\023\001\
\024\001\025\001\026\001\009\001\010\001\011\001\255\255\255\255\
\014\001\015\001\016\001\017\001\018\001\019\001\255\255\255\255\
\255\255\023\001\024\001\025\001\026\001\255\255\002\001\255\255\
\255\255\002\001\006\001\007\001\008\001\006\001\007\001\008\001\
\009\001\010\001\255\255\255\255\255\255\014\001\015\001\016\001\
\017\001\018\001\019\001\043\001\044\001\045\001\046\001\047\001\
\025\001\026\001\002\001\255\255\255\255\255\255\006\001\007\001\
\008\001\009\001\010\001\255\255\255\255\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\255\255\255\255\255\255\002\001\
\255\255\025\001\026\001\006\001\007\001\008\001\043\001\044\001\
\045\001\046\001\047\001\014\001\015\001\016\001\017\001\018\001\
\019\001\255\255\255\255\255\255\002\001\255\255\025\001\026\001\
\006\001\007\001\008\001\255\255\255\255\255\255\255\255\255\255\
\014\001\015\001\016\001\017\001\018\001\019\001\255\255\255\255\
\255\255\002\001\255\255\025\001\026\001\006\001\007\001\008\001\
\255\255\255\255\255\255\255\255\255\255\014\001\015\001\016\001\
\017\001\018\001\019\001\255\255\255\255\255\255\002\001\255\255\
\025\001\026\001\006\001\007\001\008\001\255\255\255\255\255\255\
\255\255\255\255\014\001\015\001\016\001\017\001\018\001\019\001\
\255\255\255\255\255\255\002\001\255\255\025\001\026\001\006\001\
\007\001\008\001\255\255\255\255\255\255\002\001\255\255\014\001\
\015\001\006\001\255\255\008\001\255\255\007\001\255\255\009\001\
\010\001\011\001\025\001\026\001\014\001\015\001\016\001\017\001\
\018\001\019\001\255\255\255\255\255\255\023\001\024\001\025\001\
\026\001\007\001\255\255\009\001\010\001\011\001\255\255\255\255\
\014\001\015\001\016\001\017\001\018\001\019\001\255\255\255\255\
\255\255\023\001\024\001\025\001\026\001\009\001\010\001\011\001\
\255\255\255\255\014\001\015\001\016\001\017\001\018\001\019\001\
\255\255\255\255\255\255\023\001\024\001\025\001\026\001\009\001\
\010\001\011\001\255\255\255\255\014\001\015\001\016\001\017\001\
\018\001\019\001\255\255\255\255\255\255\023\001\024\001\025\001\
\009\001\010\001\011\001\255\255\255\255\014\001\015\001\016\001\
\017\001\018\001\019\001\009\001\010\001\011\001\023\001\024\001\
\255\255\255\255\016\001\017\001\018\001\019\001\255\255\255\255\
\255\255\023\001\024\001"

let yynames_const = "\
  PAREN_L\000\
  PAREN_R\000\
  CURLY_L\000\
  CURLY_R\000\
  SQUARE_L\000\
  SQUARE_R\000\
  SEMICOL\000\
  COMMA\000\
  ADD\000\
  SUB\000\
  TIMES\000\
  ASSIGN\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  GT\000\
  LT\000\
  LEQ\000\
  GEQ\000\
  PERIOD\000\
  TRUE\000\
  FALSE\000\
  DIV\000\
  MOD\000\
  AND\000\
  OR\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  RETURN\000\
  NEW\000\
  DEL\000\
  NULL\000\
  MAT_FILL\000\
  MAT_TRANSPOSE\000\
  MAT_ROWS\000\
  MAT_COLS\000\
  MAT_EQ\000\
  MAT_ADD\000\
  MAT_MULT_SCALAR\000\
  MAT_MULT\000\
  NUM\000\
  BOOL\000\
  STRING\000\
  VOID\000\
  XIRTAM\000\
  EOF\000\
  "

let yynames_block = "\
  NUMLIT\000\
  BOOLLIT\000\
  ID\000\
  STRLIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 46 "parser.mly"
           (_1)
# 402 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                 ( ([], [])               )
# 408 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_decl) in
    Obj.repr(
# 50 "parser.mly"
                   ( ((_2 :: fst _1), snd _1) )
# 416 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func_decl) in
    Obj.repr(
# 51 "parser.mly"
                    ( (fst _1, (_2 :: snd _1)) )
# 424 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 55 "parser.mly"
                               ( (_1, _2, Empty) )
# 432 "parser.ml"
               : 'var_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
                     ( [] )
# 438 "parser.ml"
               : 'var_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var_decl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_decl) in
    Obj.repr(
# 61 "parser.mly"
                           ( _2 :: _1 )
# 446 "parser.ml"
               : 'var_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'f_args_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'var_decl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 65 "parser.mly"
     ({  typ = _1;
         f_name = _2; (*func name, use symboltables*)
         f_args = _4;(*args *)
         f_locals = _7;
         f_statements = List.rev _8  (*statements in function*)})
# 461 "parser.ml"
               : 'func_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
                  ( [] )
# 467 "parser.ml"
               : 'f_args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'f_args_list) in
    Obj.repr(
# 72 "parser.mly"
                  ( List.rev _1 )
# 474 "parser.ml"
               : 'f_args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 75 "parser.mly"
                             ( [(_1,_2,Empty)]     )
# 482 "parser.ml"
               : 'f_args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'f_args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 76 "parser.mly"
                             ( (_3,_4,Empty) :: _1 )
# 491 "parser.ml"
               : 'f_args_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
    ( [] )
# 497 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 80 "parser.mly"
                ( List.rev _1 )
# 504 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                       (    [_1]    )
# 511 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                         ( (_3 :: _1) )
# 519 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
           (Num)
# 525 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
             (Bool)
# 531 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
             (String)
# 537 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
             (Void)
# 543 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
             (Xirtam)
# 549 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
 ([])
# 555 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 96 "parser.mly"
                  (_2 :: _1)
# 563 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                                                        ( Expr _1               )
# 570 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 102 "parser.mly"
                                                        ( Return _2             )
# 577 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 105 "parser.mly"
                                                        ( Block(List.rev _2)    )
# 584 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 106 "parser.mly"
                                              ( If(_3, _5, Block([])) )
# 592 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 107 "parser.mly"
                                              ( If(_3, _5, _7)        )
# 601 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 108 "parser.mly"
                                                                    ( For(_3, _5, _7, _9)   )
# 611 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 109 "parser.mly"
                                                        ( While(_3, _5)         )
# 619 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
   (Empty)
# 625 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
         (_1)
# 632 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser.mly"
                        ( BoolLit(true)           )
# 638 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "parser.mly"
                          ( BoolLit(false)          )
# 644 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 118 "parser.mly"
                          ( StrLit(_1)              )
# 651 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 119 "parser.mly"
                            ( NumLit(_1)            )
# 658 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 120 "parser.mly"
                          ( Id(_1)                  )
# 665 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'mat) in
    Obj.repr(
# 122 "parser.mly"
                                (XirtamLit(_2))
# 672 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                          ( Binop(_1, Add,      _3) )
# 680 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                         ( Binop(_1, Sub,      _3) )
# 688 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                          ( Binop(_1, Mult,      _3) )
# 696 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                       ( Binop(_1, Div,      _3) )
# 704 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                            (Binop(_1, Mod,      _3) )
# 712 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                          ( Binop(_1, Equal,   _3) )
# 720 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                          ( Binop(_1, Neq,      _3) )
# 728 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                          ( Binop(_1, Great,     _3) )
# 736 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                          ( Binop(_1, Less,     _3) )
# 744 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                          ( Binop(_1, Geq,      _3) )
# 752 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                          ( Binop(_1, Leq,      _3) )
# 760 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                          ( Binop(_1, And,      _3) )
# 768 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                          ( Binop(_1, Or,       _3) )
# 776 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                          ( Unop(Neg, _2)           )
# 783 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                          ( Unop(Not, _2)           )
# 790 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                          ( Assign(_1, _3)          )
# 798 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 142 "parser.mly"
                                 ( Call(_1, _3)     )
# 806 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 143 "parser.mly"
                            ( _2                    )
# 813 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'args_list) in
    Obj.repr(
# 150 "parser.mly"
                                                ([XirtamLit(List.rev _2)])
# 820 "parser.ml"
               : 'mat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'args_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'mat) in
    Obj.repr(
# 151 "parser.mly"
                                                (XirtamLit(List.rev _2)::_5)
# 828 "parser.ml"
               : 'mat))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
