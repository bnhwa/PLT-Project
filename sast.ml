open Ast

 
type sexpr = typ * sx
and sx =
	(*Primitives and expressions*)
	SNumLit of float
	| SStrLit of string
	| SBoolLit of bool
  | SXirtamLit of sexpr list * int * int 
	| SId of string
	| SUnop of  op_un * sexpr
	| SBinop of sexpr * op_bin * sexpr
	| SAssign of string * sexpr 
	| SCall of string * sexpr list
  | SEmpty


type sbind = typ * string * sexpr

type sstmt = 
    SBlock of sstmt list
  (*  
  | SVDeclList of typ * (string * sexpr) list
  | SVDecl of typ * string * sexpr
*)
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt (* adding while loop back*)
  (* | SContinue *)


(*should make sbind because these should be semantically checked!*)
type sfunc_decl = {
    styp          : typ;
    sf_name       : string;
    sf_args       : sbind list; (* formals*)
    sf_locals     : sbind list; (*add local variables *)
    sf_statements : sstmt list;
  }

(* Pretty-printing functions below:*)

type sprogram = bind list * sfunc_decl list

let rec string_of_sexpr (t, e) = 
  "(" ^ string_of_typ t ^ " : " ^ (match e with
  | SNumLit(l) -> string_of_float l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SId(s) -> s
  | SStrLit(s) -> s
  (* | XirtamLit(x) -> "[" ^ String.concat "," (List.map (fun lst -> "[" ^ String.concat "," (List.map string_of_expr lst) ^ "]") x) ^ "]" *)
  (* check row col sizes in semant for SXirtamLit*)
  | SXirtamLit(x, r, c) -> "(rows: " ^ string_of_int r ^ ", cols: " ^ string_of_int c ^ ") : [" ^ String.concat ", " (List.map string_of_sexpr x) ^ "]"
  (*we use fun instead of function because fun can take in multiple arguments*)
  | SBinop(e1, o, e2) -> string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SCall(f, e) -> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr e) ^ ")"
  | SEmpty -> ""
  ) ^ ")"

let rec string_of_sstmt = function
    SBlock(stmts) -> "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  (*| Break(n) -> "break " ^ string_of_int n ^ ";\n";*) (*are we adding break?*)
  (* | SContinue -> "continue;\n"; *)
  | SIf(e, s, SBlock([])) -> "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^ string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s (* we should implement this I think*)


(* Print out argument type and argument identifier *)
let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sf_name ^ "(" ^String.concat ", " (List.map (fun (_, f_arg_name, _) -> f_arg_name) fdecl.sf_args) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.sf_locals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sf_statements) ^
  "}\n"

  
let string_of_sprogram (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)



