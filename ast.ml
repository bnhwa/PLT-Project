(*
authored by: Bailey Hwa and Shida Jing
citation: microc compiler shown in class
*)

(*unary operations*)
type op_un = Not | Neg

type op_bin = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Great | Geq | And | Or | Mod

type typ =  
   Num
  | Bool
  | String
  | Void
  | Int
  | Xirtam
 
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
  | XirtamLit of expr list
  | Empty

(* bind can be expression too*)
type bind = typ * string * expr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt (*the while loop is back in business*)

type func_decl = {
    typ : typ;
    f_name : string;
    f_args : bind list; (* formals*)
    f_locals : bind list; (* adding this, making it akin to micro c makes life easier *)
    f_statements : stmt list;
  }

  (* Pretty-printing functions below:*)

type program = bind list * func_decl list

let string_of_op = function
  Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Great -> ">"
  | Less -> "<"
  | Geq -> ">="
  | Neq -> "!="
  | Leq -> "<="
  | And -> "&&"
  | Or -> "||"
  | Mod -> "%"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
  | NumLit(l) -> string_of_float l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | XirtamLit(l) -> "\n[" ^ String.concat "," (List.map string_of_expr l) ^ "]\n"
  | Id(s) -> s
  | StrLit(s) -> s
  (*we use fun instead of function because fun can take in multiple arguments*)
  | Binop(e1, o, e2) ->string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, e) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr e) ^ ")"
  | Empty -> ""

let string_of_typ = function
    Num -> "num"
  | Bool -> "bool"
  | String -> "string"
  | Void -> "void"
  | Int -> "int" 
  | Xirtam -> "xirtam"

let string_of_bind (t, n) = string_of_typ t ^ " " ^ n ^ ";\n"

let rec string_of_stmt = function
    Block(stmts) -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^ string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s


let string_of_tuple x = "(" ^ (fst x) ^ " : " ^ string_of_typ (snd x) ^ ")"

(* variable name pretty print, some vdecl can have type, name, and optional assignment to expression, hence, _typ,_name, _*)
let string_of_vdecl (_typ, _name, _) =
  string_of_typ _typ ^ " " ^ _name ^ ";\n"

(* Print out argument type and argument identifier *)
let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.f_name ^ "(" ^ String.concat ", " (List.map (fun (_, f_arg_name, _) -> f_arg_name) fdecl.f_args) ^

  ")\n{\n"^ 
  String.concat "" (List.map string_of_vdecl fdecl.f_locals) ^
  String.concat "" (List.map string_of_stmt fdecl.f_statements) ^"}\n"

let string_of_program (vars, funcs) = 
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)