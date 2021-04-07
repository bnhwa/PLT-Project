(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

(* global semant functions*)
(* make error msg*)
let make_err er = raise (Failure er) ;;
(*if func is main, make it hidden int type, if it is somehow int and not main, return error*)
let typ_helper _nm _tp = (match _nm with
      "main" -> Int
      | _ -> (match _tp with 
          Int -> make_err ("function "^_nm ^" is of illegal type int")
          |_   -> _tp));;

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
    (* and make_err er = raise (Failure er) *)
    and n = fd.f_name (* Name of the function *)
    in
    (* ensure again that func main is int as well as return type*)
    let _ret = {
      typ = typ_helper n fd.typ;
      f_name = fd.f_name; 
      f_args = fd.f_args;
      f_locals = fd.f_locals;
      f_statements = fd.f_statements;
    } 
    in
    (* print_endline (string_of_typ _ret.typ) ; *)

    match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err
       (* add to prevent user from creating functions with same name as built-in functions? *)
       | _ when n = "printn" (* 
          || n = "fillMat"
          || n = "transpose"
          || n = "getrows"
          || n = "getcols"
          || n = "equals"
          || n = "addMat"
          || n = "multScalar"
          || n = "multMat" *)
            -> make_err dup_err  
       | _ ->  StringMap.add n _ret map 
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
  (*check local vars for duplicates!!!! check for arg duplicates and local var duplicates*)
  check_binds "function argument" func.f_args;
  check_binds "local variable" func.f_locals;
  let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in   

    (* Build local symbol table of variables for a given function *)
    (* original*)
(*     let symbols =
      List.fold_left 
        (fun _val (_type, _name, _) -> StringMap.add _name _type _val)
        StringMap.empty (globals @ func.f_args @ func.f_locals )
    in *)
    (*make symboltable have information about type, name, and whether the variable is initialized or not*)
    let symbols =
      List.fold_left 
        (fun _val (_type, _name, _) -> (StringMap.add _name {
          v_type = _type;
          v_id = _name;
          v_init  = false;
          } _val)
        )
        StringMap.empty (globals @ func.f_args @ func.f_locals )(*(globals @ func.f_args @ func.f_locals )*)
    in
    (* function args only, use to avoid program seeing function args as uninitialized *)
    let func_arg_symbols =
      List.fold_left 
        (fun _val (_type, _name, _) -> (StringMap.add _name {
          v_type = _type;
          v_id = _name;
          v_init  = false;
          } _val)
        )
        StringMap.empty (func.f_args )(*(globals @ func.f_args @ func.f_locals )*)
      in

     let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* check if vars within expressions where relevant are initialized, return e if ok
      maybe do type conversion here?? idk
      maybe we want to auto set variables to default value if they r not initialized? *)
    let expr_init_check e_in = 
      (*fix the error print*)
      let init_err _i = ("cannot use unitialized variable "^ _i ^" in expression "^ (string_of_expr e_in)) in
      let rec init_check_helper e = match e with
          NumLit  _   -> e
        | BoolLit _   -> e
        | StrLit _ -> e
        | Empty       -> e
        | XirtamLit _ -> e (*double check *)
        | Id i ->
            let var_dat = type_of_identifier i in
            if (var_dat.v_init = false ) && not (StringMap.mem i func_arg_symbols) then
                make_err (init_err i)
            else
                e
      | Call(_, args) as call ->   List.iter (fun _ex -> ignore (init_check_helper _ex)) args; e
      | Unop (_, ex) -> init_check_helper ex
      | Binop (e1, _, e2)  -> (init_check_helper e1);( init_check_helper e2);e
      | Assign (id, _) as _exp ->
          let var_dat = type_of_identifier id in 
          (*set variable as initialized! we need to have let _ = or it won't work*)
          let _ = var_dat.v_init <- true ;
          in e
      in
      init_check_helper e_in;


    in
    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        NumLit  l   -> (Num, SNumLit l)
      | BoolLit l   -> (Bool, SBoolLit l)
      | StrLit l -> (String, SStrLit l)
      | Empty       -> (Void, SEmpty)
      | XirtamLit l ->

        (*
        if matrix is at outer level, then check each to make sure they r not staggered
        Also we want to map expr to each value in the matrix

                          in c 
                            matrix**, int row, int col
        [1,2,3],[1,2,3]->   [1,2,3,1,2,3], 2, 3
        *)
        (*get list of row lengths in matrix*)
        let rec mat_length_list _mat_in =  match _mat_in with
          XirtamLit x -> List.length x :: mat_length_list (List.hd x)
          | _ -> [] 
        in
        (*given list of matrix elements, check type and return error if not*)
        let check_mat_val_type _mat_val= 
          let (_typ,_e) = expr _mat_val in
          (match _typ with
            String -> make_err("no strings allowed in matrices!")
            | Bool -> make_err("no booleans allowed in matrices!")
            (* for now, no matrices except literals allowed within each other
              this also prevents unwanted self referencing of undeclared matrices:
              xirtam mat;
              mat = [[1,2,3],[1,2,true]];

            TODO: see if variables when put into matrix literals copy by value 
                  boolean casting

            *)
            | Xirtam -> make_err("Xirtam Literals are only allowed in matrices!")
            |  _ -> expr (expr_init_check _mat_val)
          )

          
        in
        (*turn matrix into flattened single array while checking fo staggered matrix, i.e., all row must have same col length*)
        let rec check_stagger test_col = function
          XirtamLit hd::tl ->
            let row_len   = List.hd test_col in (*same column we compare it to*)
            let row_check = List.length hd in (*row we need to check*)
            if row_len != row_check then
              make_err ("No staggered Matrices allowed, rows must be same size")
            else 
              (check_stagger (List.tl test_col) hd) @ (check_stagger test_col tl)
          (*for individual row, which is list, map expr to each matrix element*)
          | _mat_row -> List.map check_mat_val_type _mat_row      
        in
        (*get list containing length of matrix rows *)
        let mat_rc = mat_length_list (XirtamLit l) in
        let _cols_check = List.tl mat_rc in (*get the rest of the cols*)
        let _rows = List.hd mat_rc in (*rows*)
        let _cols = List.hd _cols_check in (*cols*)
        (* debug print *)
        (* print_endline ("("^(string_of_int _rows) ^", " ^(string_of_int _cols)^")"); *)
        (*map expr to each of the matrix elements*)
          (
            Xirtam, 
            SXirtamLit (check_stagger _cols_check l, _rows, _cols)
          )


      | Id s       ->
          let var_dat = type_of_identifier s in
            (var_dat.v_type, SId s)
          (* (type_of_identifier s, SId s) *)
      | Call(fname, args) as call ->
          call = expr_init_check  call;
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

      | Unop (op, l) as ex ->
        let (t, l') = expr (expr_init_check l) in
          let ty = match op with
          (*double check this, *)
            Neg when t = Num -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " applied to " ^ string_of_expr ex))
        in (ty, SUnop(op, (t, l')))
      | Binop (e1, op, e2) as e ->

          let (t1, e1') = expr (expr_init_check e1) 
          and (t2, e2') = expr (expr_init_check e2) in
          (* Based on the MicroC, all binary operators require operands of the same type, 
          However, should we allow type casting between bool and num? 
          Someone look into this please
           *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div | Mod when same && t1 = Num   -> Num
          | Equal | Neq            when same               -> Bool
          | Less | Leq | Great | Geq
                     when same && (t1 = Num) -> Bool (*castable to bool, should we like python have string "" = false and "asdfasdf"  be true?*)
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
            Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))

      | Assign (id, v) as _exp ->
          let var_dat = type_of_identifier id in 
          let _left = var_dat.v_type  in
          (* let _left = type_of_identifier id in  *)
          let (_right, val') = expr (expr_init_check v) in
          let err = 
            "illegal assignment " ^ string_of_typ _left ^ " = " ^ string_of_typ _right ^ " in " ^ string_of_expr _exp
          in 
          (*set variable as initialized! we need to have let _ = or it won't work*)
          let _ = var_dat.v_init <- true ;
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
      | If(p, b1, b2) -> SIf(check_bool_expr (expr_init_check p), check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) ->
    SFor(expr (expr_init_check e1), check_bool_expr (expr_init_check e2), expr (expr_init_check e3), check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr (expr_init_check p), check_stmt s)
      | Return e -> let (t, e') = expr e in
      (match func.f_name with 
        (*The user should not give main a return value*)
        "main" -> make_err ("function main should not have a return value!")
        | _ ->
          if t = func.typ then
            SReturn (t, e') 
          else 
          make_err("return gives " ^ string_of_typ t ^ " expected " ^
       string_of_typ func.typ ^ " in function" ^ string_of_expr e)
      )



      
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

    in (* body of check_function, get expression for assignment *)


  
    let arg_helper (_type,_name,_val) = (_type,_name, expr _val)
    in
    { styp = typ_helper func.f_name func.typ;
      sf_name = func.f_name; 
      sf_args = List.map arg_helper func.f_args;
      sf_locals = List.map arg_helper func.f_locals;
      sf_statements = match check_stmt (Block func.f_statements) with
  SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in (globals, List.map check_function functions)
