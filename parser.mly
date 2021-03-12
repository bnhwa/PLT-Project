/* 
Citation: microC processor code
Data used: 
   args_list
   ordering of tokens
   fdecl
   typ
   expr_opt
*/

%{ open Ast %}

%token LEFT_PAREN RIGHT_PAREN LEFT_CURL RIGHT_CURL LEFT_BRACK RIGHT_BRACK
%token PLUS MINUS TIMES DIVIDE SEQUENCING EQUAL EOF
%token NOT MODULO EXPONENT BITWISE_AND LINE BITWISE_NOT   
%token SING_QUOTE NUMERIC STRING BOOL XIRTAM IF ELSE ELSEIF FOR VOID RETURN CONTINUE TRUE FALSE NEW DEL NULL FUNCTION CHECK_EQUAL FLOAT
%token CHECK_NOT_EQUAL SMALLER GREATER SMALLER_EQUAL GREATER_EQUAL BOOL_AND BOOL_OR DOT COMMA ASSIGN INT FLOATLIT

%token <int> LITERAL
%token <string> IDENTIFIER

%nonassoc HIGHERTHANELSE
%nonassoc ELSEIF
%nonassoc ELSE
%left ASSIGN
%left BOOL_OR
%left BOOL_AND
%left CHECK_NOT_EQUAL SMALLER GREATER SMALLER_EQUAL GREATER_EQUAL
%left SEQUENCING
%left PLUS MINUS
%left TIMES DIVIDE
%left EXPONENT
%right NOT

%start program
%type <Ast.program> program

%% 
program:
  decls EOF {None}

decls:
   {None}
   | decls variable_decl {None}
   | decls function_decl {None}

variable_decl:
   typ IDENTIFIER SEQUENCING{None}
   | typ IDENTIFIER ASSIGN expr {None}

function_decl:
   list_type FUNCTION IDENTIFIER LEFT_PAREN args_list RIGHT_PAREN LEFT_BRACK decl_list RIGHT_BRACK {None}

decl_list:
    /* nothing */ { None }
  | decl_list variable_decl { None }
  | decl_list stmt { None }

expr:
     LITERAL            { None }
   | IDENTIFIER         { None }
   | FLOATLIT           { None }
   | TRUE               { None }
   | FALSE              { None }
   | expr PLUS   expr   { None }
   | expr MINUS  expr   { None }
   | expr TIMES  expr   { None }
   | expr DIVIDE expr   { None }
   | expr CHECK_NOT_EQUAL expr       { None }
   | expr EXPONENT expr              { None }
   | expr SMALLER expr               { None }
   | expr GREATER expr               { None }
   | expr SMALLER_EQUAL expr         { None }
   | expr GREATER_EQUAL expr         { None }
   | expr BOOL_AND expr              { None }
   | expr BOOL_OR expr               { None }
   | NOT expr                        { None }
   | MINUS expr                      { None } 
   | XIRTAM IDENTIFIER ASSIGN NEW XIRTAM LEFT_PAREN set_of_sets RIGHT_PAREN {None}
   | XIRTAM IDENTIFIER ASSIGN NEW XIRTAM LEFT_PAREN tuple_type_size RIGHT_PAREN {None}  
   | XIRTAM IDENTIFIER ASSIGN NEW XIRTAM LEFT_PAREN args_opt RIGHT_PAREN {None} 
   | IDENTIFIER DOT IDENTIFIER LEFT_PAREN args_opt RIGHT_PAREN {None}
   | XIRTAM IDENTIFIER ASSIGN NEW XIRTAM DOT IDENTIFIER LEFT_PAREN args_opt RIGHT_PAREN {None}
   | IDENTIFIER LEFT_PAREN args_opt RIGHT_PAREN { None }


stmt:
   expr SEQUENCING expr {None}
   | RETURN expr_opt SEQUENCING {None}
   | IF LEFT_PAREN expr RIGHT_PAREN LEFT_CURL stmt RIGHT_CURL HIGHERTHANELSE {None}
   | IF LEFT_PAREN expr RIGHT_PAREN LEFT_CURL stmt RIGHT_CURL expr HIGHERTHANELSE{None}
   | ELSEIF LEFT_PAREN expr RIGHT_PAREN LEFT_CURL stmt RIGHT_CURL expr HIGHERTHANELSE{None}
   | ELSE LEFT_CURL stmt RIGHT_CURL {None}
   | FOR LEFT_PAREN expr_opt SEQUENCING expr_opt SEQUENCING expr_opt RIGHT_PAREN LEFT_BRACK stmt RIGHT_BRACK {None}

args_list:
    expr {None}
   | expr COMMA args_list {None}

set:
   LEFT_BRACK args_list RIGHT_BRACK {None}

set_of_sets:
   set {None}
   | set COMMA set_of_sets {None}

tuple_type_size:
   enum_matrix_type COMMA LITERAL {None}

enum_matrix_type:
   "identity" {None}
   | "zeros" {None}
   | "null" {None}

function_decl:
   typ FUNCTION IDENTIFIER LEFT_PAREN args_opt RIGHT_PAREN LEFT_CURL expr RIGHT_CURL {None}

args_opt:
    /* nothing */ { None }
  | args_list  { None }

typ:
   INT {None}
  | BOOL    {None}
  | FLOAT   {None}
  | STRING {None}
  | VOID    {None}
  | XIRTAM  {None}

list_type:
   typ{ None}
   | typ COMMA list_type {None}

expr_opt:
   {None}
   | expr {None}


