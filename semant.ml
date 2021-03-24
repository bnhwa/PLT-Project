(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =


  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void;
      f_ret = true;
      f_name = name; 
      f_args = [(ty, "x")];
      f_statements = []; } map
    in List.fold_left add_bind StringMap.empty [ ("print", String)]
  in

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.f_name ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.f_name
    and make_err er = raise (Failure er)
    and n = fd.f_name (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

    (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_function func =

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        NumLit  l -> (Num, SNumLit l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | Empty     -> (Void, SEmpty)
      | Id s       -> (type_of_identifier s, SId s)
      | Call(fname, args) as call ->  (fd.typ, SCall(fname, args))
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | Block sl -> 
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl)

    in (* body of check_function *)
    { styp = func.typ;
      sf_ret = func.sf_ret; 
      sf_name = func.f_name;
      sf_args = func.sf_args;
      sf_statements = match check_stmt (Block func.f_statements) with
	SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in (globals, List.map check_function functions)
