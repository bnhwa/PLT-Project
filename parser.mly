
/* 
XIRTAM Parser alternate version
Citation: microC processor code
Data used: 
   args_list
   ordering of tokens
   fdecl
   typ
   expr_opt
*/

%{ open Ast %}
/* Tokens: syntax */
%token PAREN_L PAREN_R CURLY_L CURLY_R SQUARE_L SQUARE_R SEMICOL COMMA
/* Tokens: Operators & literals */
%token ADD SUB TIMES  DIV ASSIGN NOT EQ NEQ GT LT LEQ GEQ PERIOD TRUE FALSE
/* Tokens: program flow */
%token AND OR IF ELSE ELSEIF FOR RETURN CONTINUE NEW DEL NULL /*BREAK*/
/* Tokens: matrix functions */
%token MAT_FILL MAT_TRANSPOSE MAT_ROWS MAT_COLS MAT_EQ MAT_ADD MAT_MULT_SCALAR MAT_MULT /*MAT_IDENTITY*/
/* Tokens: Datatypes */
%token XIRTAM NUM BOOL STRING FUNC VOID 

/*Literals*/
%token <float> NUMLIT
%token <bool> BOOLLIT
%token <string> ID
%token <string> STRLIT
%token EOF

/*Program*/
%start program
%type <Ast.program> program
%nonassoc HTELSE
%nonassoc ELSEIF
%nonassoc ELSE
%left ASSIGN
%left COMMA
%left OR
%left AND
%left EQ NEQ
%left GT LT GEQ LEQ
%left ADD SUB
%left TIMES DIV 
%right NOT NEG /*boolean negation and negative*/

%%

program:
	decls EOF {$1}
decls:
 	| decls var_decl { (($2 :: one $1), two $1) }
 	| decls func_decl { (one $1, ($2 :: two $1)) }

var_decl:
   typ ID SEMICOL { ($1, $2) }

func_decl:
 	typ ID PAREN_L f_args_opt PAREN_L CURLY_L stmt_list CURLY_R
     {{  typ = $1;
         f_ret = ($1 = Func);(* check if function is void or not*)
         f_id = $2;(*func name, use symboltables*)
         f_args = $4;(*args *)
         f_statments = List.rev $7  (*statements in function*)}}/*reverse list to ensure proper ordering*/
f_args_opt:
   	{ [] }
 	| f_args_list   { List.rev $1 }

f_args_list: /* arg list with types for functinos */
    typ ID                   { [($1,$2)]     }
 	| f_args_list COMMA typ ID { ($3,$4) :: $1 }

args_opt:
   	{ [] }
 	| args_list   { List.rev $1 }

args_list: 
      expr             {    [$1]    }
 	| args_list COMMA expr { ($3 :: $1) }

  /*datatypes*/
typ:
    NUM		  {Num}
  	| BOOL    {Bool}
  	| STRING  {String}
  	| VOID    {Void}
	| xirtam_typ  {Xirtam}/* do */
	| FUNC    {Func}
  /*if we are doing matrix of things other than numerics, use below*/
  /*
mat_typ:
    NUM     {Num}
    | BOOL    {Bool}
    | STRING  {String}
  */



xirtam_typ:
/*MAKE SURE TO DO TYPE CONVERSIONS AND ERROR CHECK SINCE XIRTAM ONLY USES NUM TYPES*/
  XIRTAM SQUARE_L NUMLIT SQUARE_R SQUARE_L NUMLIT SQUARE_R  {Xirtam( int_of_float $3, int_of_float $6)}
/* mat_typ Xirtam SQUARE_L NUMLIT SQUARE_R SQUARE_L NUMLIT SQUARE_R  {Xirtam($1, $4,$7)}*/

var_decl_stmts:
    ID             {[($1, Empty)]}
  	| ID ASSIGN expr {[($1, $3)]}
  	| var_decl_stmts COMMA ID             {($3, Empty) ::$1}/*just variables, no assignment*/
  	| var_decl_stmts COMMA ID ASSIGN expr {($3, $5):: $1}/*assignment to expression variables*/

stmt_list:
	{[]}
	| stmt_list stmt {$2 :: $1}
stmt: /*all statements must end with semicolon*/
	typ var_decl_stmts SEMICOL                 { VDeclList($1, List.rev $2)  }
  	| expr SEMICOL                               { Expr $1               }
  	| RETURN expr_opt SEMICOL                    { Return $2             }
  	| CONTINUE SEMICOL                           { Continue              }
  	/*| BREAK SEMICOL                              { Break               } *//*are we going to implement this?*/
  	| CURLY_L stmt_list CURLY_R                  { Block(List.rev $2)    } 
	| IF PAREN_L expr PAREN_R stmt ELSE stmt    { If($3, $5, Block([]))       } /*add if else block*/
	| ELSEIF PAREN_L expr PAREN_R stmt HTELSE stmt { Elseif ($3, $5, $7)} /* DOUBLE CHECK THIS*/
  	| FOR PAREN_L expr_opt SEMICOL expr SEMICOL expr_opt PAREN_R stmt { For($3, $5, $7, $9)   }

expr_opt:
  	{Empty} /*no expression or something */
	|  expr {$1}

expr:
  	TRUE                 { BoolLit(true)           }
  	| FALSE                { BoolLit(false)          }
  	| STRLIT               { StrLit($1)              }
  	| NUMLIT                 { NumLit($1)            }
  	| ID                   { Id($1)                  }
  	| expr ADD       	expr { Binop($1, Add,      $3) }
  	| expr SUB      	expr { Binop($1, Sub,      $3) }
  	| expr TIMES      expr { Binop($1, Mul,      $3) }
  	| expr DIV   		expr { Binop($1, Div,      $3) }
  	| expr EQ         expr { Binop($1, Equals,   $3) }
  	| expr NEQ        expr { Binop($1, Neq,      $3) }
  	| expr GT         expr { Binop($1, Greater,     $3) }
  	| expr LT         expr { Binop($1, Less,     $3) }
  	| expr GEQ        expr { Binop($1, Geq,      $3) }
  	| expr LEQ        expr { Binop($1, Leq,      $3) }
  	| expr AND        expr { Binop($1, And,      $3) }
  	| expr OR         expr { Binop($1, Or,       $3) }
  	| SUB expr %prec  NEG	 { Unop(Neg, $2)           }/*minus statement, adding prec made it work,*/
  	| NOT expr             { Unop(Not, $2)           }/*logical negation*/
  	| ID ASSIGN expr       { Assign($1, $3)          }
  	| ID PAREN_L args_opt PAREN_R { Call($1, $3)     }
  	| PAREN_L expr PAREN_R   { $2                    }/*grouping func1((a+b)(b+c))*/
   	/*xirtam matrices declare, get, and assign IMPLEMENT OTHER*/
   	/*
	TODO:

	MAYBE LIST STUFF? 
	Focus rn on getting the core functionality and matrix operations
   	*/
    /* | NEW XIRTAM xirtam_dec {XirtamDec_lit($3)}  matrix literal: {1,2,3,4,5} */
    | SQUARE_L xirtam_row SQUARE_R 	{XirtamLit(List.rev $2)}
  	| ID SQUARE_L expr SQUARE_R SQUARE_L expr SQUARE_R { XirtamGet($1, int_of_float $3, int_of_float $6) }/*mat1[0][2]*/
  	| ID SQUARE_L expr SQUARE_R SQUARE_L expr SQUARE_R ASSIGN expr { XirtamAssign($1, int_of_float $3, int_of_float $6, $9) }
    /*mat1[0][2]=expr*/
  	/*| XIRTAM PERIOD PAREN_L xirtam_funcs PAREN_R {$3}*//*xirtam funcs*/
  	/*double check how things are initialized and how we want to do matrix functions*/
   	/*check type conversion int bc scanner default converts all int/floats to float!!!!! 
	I put int_of_float to ensure certain arguments are ints, with matrix indexing and assignment
   	*/
	/*Xirtam matrix functions*/

	/*| MAT_IDENTITY		PAREN_L expr PAREN_R {XirtamIdentity($3) }make identity matrix of size */
	| MAT_TRANSPOSE		PAREN_L expr PAREN_R {XirtamTranspose($3) }
	| MAT_ROWS 			PAREN_L expr  PAREN_R {XirtamRows($3)}
	| MAT_COLS 			PAREN_L expr PAREN_R {XirtamCols($3)}
	| MAT_FILL 			PAREN_L expr COMMA expr PAREN_R {XirtamFill($3,$5) } 
	| MAT_EQ			PAREN_L expr COMMA expr PAREN_R {XirtamEq($3,$5) } 
	| MAT_ADD			PAREN_L expr COMMA expr PAREN_R {XirtamAdd($3,$5) } 
	| MAT_MULT_SCALAR 	PAREN_L expr COMMA expr PAREN_R {XirtamMultScalar($3,$5) } 
	| MAT_MULT 			PAREN_L expr COMMA expr PAREN_R {XirtamMult($3,$5) } 

xirtam_dec:

	/*CURLY_L xirtam_row CURLY_R { XirtamDec_lit(List.rev $2) }DOUBLE CHECK THIS*/
	/* | PAREN_L expr COMMA expr PAREN_R { XirtamDec_rc(int_of_float $2, int_of_float$4) } */ /*new Xirtam(1,2) , default zeros fill.
	IN CODEGEN MAKE SURE row, col VALS >0*/
	/*| PAREN_L STRLIT COMMA expr PAREN_R { XirtamDec_str($2, int_of_float $4) }new Xirtam("identity",3) : 3x3 identity*/

/* xirtam_row: */
  	 /* SQUARE_L args_opt SQUARE_R { [$2]} {1,2,3}  {{1},{2}} */
 	/* | xirtam_row COMMA SQUARE_L args_opt SQUARE_R  { ($4 :: $1) } */
