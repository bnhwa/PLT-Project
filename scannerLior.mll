{ open Parser }

rule tokenize = 
parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| ';' {SEQUENCING}
| '=' {ASSIGN}
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| ['a'-'z''A'-'Z''0'-'9''_']+ as var { IDENTIFIER (var) }
| eof { EOF }
| '!' {NOT}
| '%' {MODULO}
| '^' {EXPONENT}
| '&' {BITWISE_AND}
| '(' {LEFT_PAREN}
| ')' {RIGHT_PAREN}
| '{' {LEFT_CURL}
| '}' {RIGHT_CURL}
| '|' {LINE}
| '~' {BITWISE_NOT}
| '[' {LEFT_BRACK}
| '\\' {RIGHT_BRACK}
| "'" {SING_QUOTE}
| "numeric" {NUMERIC}
| "string" {STRING}
| "bool" {BOOL}
| "Xirtam" {XIRTAM}
| "if" {IF}
| "else" {ELSE}
| "else if" {ELSEIF}
| "for" {FOR}
| "void" {VOID}
| "return" {RETURN}
| "continue" {CONTINUE}
| "new" {NEW}
| "del" {DEL}
| "NULL" {NULL}
| "function" {FUNCTION}
| "==" { CHECK_EQUAL }
| "!=" { CHECK_NOT_EQUAL }
| '<' { SMALLER }
| '>' { GREATER }
| ">=" { GREATER_EQUAL }
| "<=" { SMALLER_EQUAL }
| "&&" { BOOL_AND }
| "||" { BOOL_OR }
| '.' { DOT }
| ',' { COMMA }
| ['a'-'z']['.']['0'-'9']+ {FLOATLIT}
| "int" {INT}
| "string" {STRING}
| "float" {FLOAT}