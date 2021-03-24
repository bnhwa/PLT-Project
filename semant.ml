(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =
  (*global symbol table *)
  let glob_table = Hashtbl.create 10 in

  (* function binding checks, shouldnt have duplicated of that

  *)
  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void;
      f_name = name; 
      f_args = [(ty, "x")]; (*List.mapi (fun arg_num f_args_type -> (f_args_type, "x" ^ string_of_int arg_num)) f_args_type;*)
      f_statements = []; } map
    in List.fold_left add_bind StringMap.empty [ ("print", String);
    ("printn", Num);]
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
       (* add to prevent user from creating functions with same name as built-in functions? *)
       | _ when n = "print" (* 
          || n = "fillMat"
          || n = "transpose"
          || n = "getrows"
          || n = "getcols"
          || n = "equals"
          || n = "addMat"
          || n = "multScalar"
          || n = "multMat" *)
            -> make_err dup_err  
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

  let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in   

      (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                StringMap.empty (globals @ func.f_args )
    in

     let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        NumLit  l -> (Num, SNumLit l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | Empty     -> (Void, SEmpty)
      | Id s       -> (type_of_identifier s, SId s)
      | Call(fname, args) as call ->  
          let fd = find_func fname in
          let param_length = List.length fd.f_args in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = expr e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.f_args args
          in (fd.typ, SCall(fname, args'))
      | StrLit l -> (Num, SNumLit 0.)
      | Unop (op, l) -> (Num, SNumLit 0.)
      | Binop (e1, op, e2) -> (Num, SNumLit 0.)
        (* we should have binary operators be the same type? or maybe cast them?*)
      | Assign (id, v) -> (Num, SNumLit 0.)
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | Block sl -> 
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         ->
              check_stmt s :: check_stmt_list ss
              (* (*if comme*)
                

              *)
            | []              -> []
          in SBlock(check_stmt_list sl)
      | Continue -> SContinue
      | Return e -> let (t, e') = expr e in
        if t = func.typ then SReturn (t, e') 
        else raise (
	  Failure ("return gives " ^ string_of_typ t ^ " expected " ^
		   string_of_typ func.typ ^ " in " ^ string_of_expr e))

    in (* body of check_function *)
    { styp = func.typ;
      sf_name = func.f_name;
      sf_args = func.f_args;
      sf_statements = match check_stmt (Block func.f_statements) with
	SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in (globals, List.map check_function functions)
