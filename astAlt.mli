(*bnh2128

citation: microc compiler shown in class
*)

(*unary operations*)
type op_un = Not | Neg

type op_bin = Add | Sub | Mult | Div | Mod | Exponent | Equal | Neq | Less | Leq | Great | Geq | And | Or

type bind = typ * string

type program = bind list * func_decl list

type typ = 
  Xirtam of typ * int * int
  | Num
  | Bool
  | String
  | Void
  | Func

type expr =
	(*Primitives and expressions*)
	NumLit of float
	| StrLit of string
	| BoolLit of bool
	| Id of string
	| Unop of  op_un * expr
	| Binop of expr * op_bin * expr
	| Assign of string * expr 
	| Call of string * expr list
	(*IMPLEMENT Xirtam specific below*)
  | XirtamDec_lit of expr list list
  | XirtamDec_rc of int int
  | XirtamGet of string * expr * expr
  | XirtamAssign of string * expr * expr * expr
(* maybe have these already in the function dictionary?
  | MAT_TRANSPOSE   PAREN_L expr PAREN_R {XirtamTranspose($3) }
  | MAT_ROWS      PAREN_L expr  PAREN_R {XirtamRows($3)}
  | MAT_COLS      PAREN_L expr PAREN_R {XirtamCols($3)}
  | MAT_FILL      PAREN_L expr COMMA expr PAREN_R {XirtamFill($3,$5) } 
  | MAT_EQ      PAREN_L expr COMMA expr PAREN_R {XirtamEq($3,$5) } 
  | MAT_ADD     PAREN_L expr COMMA expr PAREN_R {XirtamAdd($3,$5) } 
  | MAT_MULT_SCALAR   PAREN_L expr COMMA expr PAREN_R {XirtamMultScalar($3,$5) } 
  | MAT_MULT      PAREN_L expr COMMA expr PAREN_R {XirtamMult($3,$5) } 
*)
  | Empty


type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  (*| While of expr * stmt *) (*are we implementing this?*)

type func_decl = {
    typ : typ;
    f_ret : bool;
    f_name : string;
    f_args : bind list;(* formals*)
    (*locals : bind list; (*local vars?*)*) 
    f_statments : stmt list;
  }

  (* Pretty-printing functions below:*)

let string_of_op = function
  Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Greater -> ">"
  | Less -> "<"
  | Geq -> ">="
  | Neq -> "!="
  | Leq -> "<="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
  | NumLit(l) -> string_of_float l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | StrLit(s) -> s
  | XirtamLit(x) -> "[" ^ String.concat "," (List.map (fun lst -> "[" ^ String.concat "," (List.map string_of_expr lst) ^ "]") x) ^ "]"
  (*we use fun instead of function because fun can take in multiple arguments*))
  | XirtamGet(id, e1, e2) -> id ^ "[" ^ string_of_expr e1 ^ "][" ^ string_of_expr e2 ^ "]"
  | XirtamAssign(id, e1, e2, e3) -> id ^ "[" ^ string_of_expr e1 ^ "][" ^
                                 string_of_expr e2 ^ "] = " ^ string_of_expr e3
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Binop(e1, o, e2) ->string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, e) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr e) ^ ")"
  | Empty -> ""

let string_of_typ = function
    Num -> "num"
  | Bool -> "bool"
  | Float -> "float"
  | String -> "string"
  | Void -> "void"
  (* maybe have matrix of different types??? errorcheck type is rights*) 
  (*
  STILLL WORKING ON THIS PART BELOW 




  *)
  | Xirtam(r, c)  -> let r_i = string_of_int r in let c_i = string_of_int 
   "matrix (r:" ^ string_of_int r ^ ", c:" ^ string_of_int c ^")"
  (* | Array -> "array" *)

let string_of_var_decl_list (n, e) =
  let suffix ex =
    if ex = Empty then ""
    else " = " ^ (string_of_expr ex)
  in n ^ (suffix e)

let string_of_bind (t, n) = string_of_typ t ^ " " ^ n ^ ";\n"

let rec string_of_stmt = function
    Block(stmts) -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  (*| Break(n) -> "break " ^ string_of_int n ^ ";\n";*) (*are we adding break?*)
  | Continue -> "continue;\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  (*| While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s*)(*are we implemeting this?*)
  | VDecl(t, n, e) -> string_of_typ t ^ " " ^ n ^
      (if e = Empty then "" else " = " ^ string_of_expr e) ^ ";\n"
  | VDeclList(t, decls) ->
      string_of_typ t ^ " " ^ String.concat ", " (List.map string_of_var_decl_list decls) ^ ";\n"

let string_of_tuple x = "(" ^ (fst x) ^ " : " ^ string_of_typ (snd x) ^ ")"

(* Print out argument type and argument identifier *)
let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.f_name ^ "(" ^ String.concat ", " (
    List.map string_of_tuple (
      List.combine (List.map snd fdecl.f_args) (List.map fst fdecl.f_args)
      )
    ) ^
  ")\n{\n" ^ String.concat "" (List.map string_of_stmt fdecl.f_statments) ^"}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_bind vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)