
/* 
XIRTAM Parser alternate
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
%token ADD SUB TIMES DIV MOD EXP ASSIGN NOT EQ NEQ GT LT LEQ GEQ PERIOD
/* Tokens: program flow */
%token BOOL_AND BOOL_OR IF ELSE ELSEIF FOR RETURN CONTINUE NEW DEL NULL 
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