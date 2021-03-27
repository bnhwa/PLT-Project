(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* function binding checks, shouldnt have duplicated of that*)
  let check_binds (kind : string) (binds : bind list) =
    List.iter (function
  (Void, b, _) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      | ((_,n1,_) :: (_,n2,_) :: _) when n1 = n2 ->
    raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a,_) (_,b,_) -> compare a b) binds)
  in 
  (*  Check global variables *)
  check_binds "global" globals;


  (* Collect function declarations for built-in functions: no bodies *)
  (*
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void;
      f_name = name; 
      f_args = [(ty, "x")]; (*List.mapi (fun arg_num f_args_type -> (f_args_type, "x" ^ string_of_int arg_num)) f_args_type;*)
      f_statements = []; } map
    in List.fold_left add_bind StringMap.empty [ ("print", String);
    ("printn", Num);]
  in
  *)
(*function declaration, add funcs and appropriate field data to the StringMap, Struct below matches that of function in ast *)
let built_in_decls =
    let add_bind map_in (_name, _argtype, _ret_type) = 
    (* add entry for function name into the string map *)
    StringMap.add _name 
      { 
        typ = _ret_type; (*type *)
        f_name = _name;
        f_args = (* create list of args *)
          (
            let rec bind_funcs = (function
              [] -> []
              | fst::snd -> (fst, "x", Empty)::(bind_funcs snd))
            in 
            bind_funcs _argtype
          );
        f_locals = [];
        f_statements = [] 
      } map_in
    in List.fold_left add_bind StringMap.empty [
      (*build in functions:  _name, [_argument_types], return types*)
     ("printn", [Num], Void);
     ]
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

    (* Build local symbol table of variables for a given function *)
    let symbols =
      List.fold_left 
        (fun _val (_type, _name, _) -> StringMap.add _name _type _val)
        StringMap.empty (globals @ func.f_args @ func.f_locals )
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
          (*call func*)
          let _fd_func_args = List.map (fun (_type, f_arg_name, _) -> (_type, f_arg_name) ) fd.f_args in
          let args' = List.map2 check_call _fd_func_args args
          in (fd.typ, SCall(fname, args'))

          (*
          let args' = List.map2 check_call fd.f_args args
          in (fd.typ, SCall(fname, args')) *)
      | StrLit l -> (Num, SNumLit 0.)
      | Unop (op, l) -> (Num, SNumLit 0.)
      | Binop (e1, op, e2) -> (Num, SNumLit 0.)
        (* we should have binary operators be the same type? or maybe cast them?*)
      | Assign (id, v) as _exp ->
          let _left = type_of_identifier id in 
          let (_right, val') = expr v in
          let err = 
            "illegal assignment " ^ string_of_typ _left ^ " = " ^ string_of_typ _right ^ " in " ^ string_of_expr _exp
          in (check_assign _left _right err, SAssign(id, (_right, val')))
    in
    (* check boolean statement*)
    let check_bool_expr e = 
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in
    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) ->
    SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = expr e in
        if t = func.typ then SReturn (t, e') 
        else raise (
    Failure ("return gives " ^ string_of_typ t ^ " expected " ^
       string_of_typ func.typ ^ " in " ^ string_of_expr e))
      
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl)

    in (* body of check_function *) (* body of check_function *)
    let arg_helper (_type,_name,_val) = (_type,_name, expr _val)
    in
    { styp = func.typ;
      sf_name = func.f_name; 
      sf_args = List.map arg_helper func.f_args;
      sf_locals = List.map arg_helper func.f_locals;
      sf_statements = match check_stmt (Block func.f_statements) with
  SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in (globals, List.map check_function functions)
