/* 
XIRTAM Parser alternate version
Citation: microC processor code
*/
	  

%{ open Ast %} 


/* Tokens: syntax */
%token PAREN_L PAREN_R CURLY_L CURLY_R SQUARE_L SQUARE_R SEMICOL COMMA
/* Tokens: Operators & literals */
%token ADD SUB TIMES ASSIGN NOT EQ NEQ GT LT LEQ GEQ PERIOD TRUE FALSE DIV MOD/**/
/* Tokens: program flow */
%token AND OR IF ELSE FOR WHILE RETURN NEW DEL NULL /*BREAK CONTINUE*/
/* Tokens: matrix functions */
%token MAT_FILL MAT_TRANSPOSE MAT_ROWS MAT_COLS MAT_EQ MAT_ADD MAT_MULT_SCALAR MAT_MULT /*MAT_IDENTITY*/
/* Tokens: Datatypes */
%token NUM BOOL STRING VOID XIRTAM

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
%nonassoc ELSE
%left ASSIGN
%left COMMA
%left OR
%left AND
%left EQ NEQ
%left GT LT GEQ LEQ
%left ADD SUB
%left TIMES DIV MOD
%right NOT NEG /*boolean negation and negative*/

%%

program:
	decls EOF {$1}
  
decls:
   /* nothing */ { ([], [])               }
 	| decls var_decl { (($2 :: fst $1), snd $1) }
 	| decls func_decl { (fst $1, ($2 :: snd $1)) }


var_decl:
  typ ID SEMICOL               { ($1, $2, Empty) }
  /* | typ ID ASSIGN expr SEMICOL { ($1, $2, Assign($2, $4))} */


var_decl_list:
    /* nothing */    { [] }
  | var_decl_list var_decl { $2 :: $1 }

func_decl:
 	typ ID PAREN_L f_args_opt PAREN_R CURLY_L var_decl_list stmt_list CURLY_R
     {{  typ = $1;
         f_name = $2; (*func name, use symboltables*)
         f_args = $4;(*args *)
         f_locals = $7;
         f_statements = List.rev $8  (*statements in function*)}}/*reverse list to ensure proper ordering*/
f_args_opt:
   /* nothing */  { [] }
 	| f_args_list   { List.rev $1 }

f_args_list: /* arg list with types for functinos */
    typ ID                   { [($1,$2,Empty)]     } /*added the Empty because we can have assignment to expression, but we dont want to in this case*/
 	| f_args_list COMMA typ ID { ($3,$4,Empty) :: $1 }

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
    /*matrix*/
    | XIRTAM {Xirtam}/*SQUARE_L NUMLIT SQUARE_R SQUARE_L NUMLIT SQUARE_R {Xirtam($3, $6)}*/

stmt_list:
	{[]}
	| stmt_list stmt {$2 :: $1}


stmt: /*all statements must end with semicolon*/
	/*typ var_decl_stmts SEMICOL                 { VDeclList($1, List.rev $2)  }*/
  expr SEMICOL                                          { Expr $1               }
  | RETURN expr_opt SEMICOL                             { Return $2             }
  /*| CONTINUE SEMICOL                                    { Continue              }*/
  /*| BREAK SEMICOL                              { Break               } *//*are we going to implement this?*/
  | CURLY_L stmt_list CURLY_R                           { Block(List.rev $2)    }
  | IF PAREN_L expr PAREN_R stmt %prec HTELSE { If($3, $5, Block([])) }
  | IF PAREN_L expr PAREN_R stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR PAREN_L expr_opt SEMICOL expr SEMICOL expr_opt PAREN_R stmt { For($3, $5, $7, $9)   }
  | WHILE PAREN_L expr PAREN_R stmt                     { While($3, $5)         }

expr_opt:
  	{Empty} /*no expression or something */
	|  expr {$1}

expr:
  	TRUE                 { BoolLit(true)           }
  	| FALSE                { BoolLit(false)          }
  	| STRLIT               { StrLit($1)              }
  	| NUMLIT                 { NumLit($1)            }
  	| ID                   { Id($1)                  }
    /*matrix  WARNING*/
    | SQUARE_L   mat   SQUARE_R {XirtamLit($2)}

  	| expr ADD       	expr { Binop($1, Add,      $3) }
  	| expr SUB      	expr { Binop($1, Sub,      $3) }
  	| expr TIMES      expr { Binop($1, Mult,      $3) }
  	| expr DIV   		expr { Binop($1, Div,      $3) }
    /* optional, i think we should implement these */
    | expr MOD        expr  {Binop($1, Mod,      $3) }
    /*| expr EXP        expr  {Binop($1, Exp,      $3) } */
  	| expr EQ         expr { Binop($1, Equal,   $3) }
  	| expr NEQ        expr { Binop($1, Neq,      $3) }
  	| expr GT         expr { Binop($1, Great,     $3) }
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




/*Xirtam matrix*/
mat:
  SQUARE_L        args_list   SQUARE_R          {[XirtamLit(List.rev $2)]}    /*[[1,2,3]]*/
  | SQUARE_L      args_list SQUARE_R COMMA mat  {XirtamLit(List.rev $2)::$5}/*[[1,2,3],          [1,2,3],MAT    ]*/