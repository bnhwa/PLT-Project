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
  | MAT_NUM_ROWS
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

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
