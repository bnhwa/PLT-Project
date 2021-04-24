
(* TODO: 
    1. Deal with strings overall.
    2. Examine line 68 - 116.
    3. There are two SBinops in the original MicroC. Did I copy the right one?
    4. Printf definition needs revision
    5. Find a way to deal with the llargs and result having the optional 3rd parameter lines 156-161,
     test first, and if it doesnt work, maybe use an ignore?  
 *)

module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)


(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
    let context    = L.global_context () in
    let llmem = L.MemoryBuffer.of_file "matrix.bc" in
    let llm = Llvm_bitreader.parse_bitcode context llmem in

    let the_module = L.create_module context "xirtam" in

    (* Get types from the context *)
    (* i1 is for Boolean *)
    let i1_t       = L.i1_type     context (*i1_t = context.i1_type *)
    and float_t    = L.double_type context
    and i32_t      = L.i32_type    context
    and i8_t       = L.i8_type     context
    in 
    let char_point_t = L.pointer_type i8_t
    and void_t     = L.void_type   context
    and xirtam_t     = L.pointer_type (match L.type_by_name llm "struct.matrix" with
      None -> raise (Failure "Missing implementation for struct Matrix")
    | Some t -> t)
     in
 
    let ltype_of_typ = function 
        A.Num  -> float_t
    |   A.Bool -> i1_t 
    |   A.Void -> void_t
    |   A.String -> char_point_t
    |   A.Int    -> i32_t
    |   A.Xirtam -> xirtam_t 
    in 


    let global_vars : L.llvalue StringMap.t =
    (* ignore 3rd optional expr*)
    let global_var m (t, n, _) = 
      let init = match t with
          A.Num -> L.const_float (ltype_of_typ t) 0.0
        (* | A.String -> *)
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in


    let printf_t : L.lltype = 
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    (*print num aka a float*)
    let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in

    let printMatrix_t = L.function_type i32_t [| xirtam_t |] in
    let printMatrix_f = L.declare_function "display" printMatrix_t the_module in
    let matrix_init_t = L.function_type xirtam_t [|i32_t ; i32_t|] in
    let matrix_init_f = L.declare_function "initMatrix_CG" matrix_init_t the_module in
    let store_matrix_t = L.function_type xirtam_t [|xirtam_t ; float_t |] in
    let store_matrix_f = L.declare_function "storeVal" store_matrix_t the_module in

    (*get  matrix m*,float r, float c                  r and c get casted in private calls*)
    let get_matrix_el_t = L.function_type float_t [| xirtam_t;float_t;float_t |] in
    let get_matrix_el_f = L.declare_function "pub_get" get_matrix_el_t the_module in
    (*set  matrix m* float r, float c, float v         r and c get casted in private calls*)
    let set_matrix_el_t = L.function_type i32_t [| xirtam_t;float_t;float_t;float_t |] in
    let set_matrix_el_f = L.declare_function "pub_set" set_matrix_el_t the_module in

    let mult_matrix_t = L.function_type xirtam_t [|xirtam_t; xirtam_t|] in
    let mult_matrix_f = L.declare_function "matrixMult" mult_matrix_t the_module in
    let add_matrix_t = L.function_type xirtam_t [|xirtam_t; xirtam_t|] in
    let add_matrix_f = L.declare_function "mAdd" add_matrix_t the_module in


    (* Define each function (arguments and return type) so we can 
        call it even before we've created its body *)
    let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
      let function_decl m fdecl =
        let name = fdecl.sf_name
        and formal_types = (* make this filter out the 2nd and 3rd arg*)
    Array.of_list (List.map (fun (t,_,_) -> ltype_of_typ t) fdecl.sf_args)

        in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
        StringMap.add name (L.define_function name ftype the_module, fdecl) m in
      List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sf_name function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p;
	let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
	StringMap.add n local m 

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m 
      in
      (*
      we should reshape these to get rid of the optional 3rd value that we don't care about in the argument list
      when adding local vars or the func vars
      *)
      let filter_args_helper (_type, _var_id, _) = (_type, _var_id) in 
      let formatted_args   = List.map filter_args_helper fdecl.sf_args in
      let formatted_locals = List.map filter_args_helper fdecl.sf_locals in
      let formals          = List.fold_left2 add_formal StringMap.empty formatted_args
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals formatted_locals

    in


    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in



        (* Construct code for an expression; return its value *)
    let rec expr builder ((_, e) : sexpr) = match e with
        SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
	    | SNumLit l  -> L.const_float float_t l
      | SStrLit s ->  L.build_global_stringptr s "tmp" builder
      | SId id -> L.build_load (lookup id) id builder
      | SXirtamLit (contents, rows, cols) ->
              let rec expr_list = function
                [] -> []
                | hd::tl -> expr builder hd::expr_list tl
              in
              let contents' = expr_list contents
              in
              let m = L.build_call matrix_init_f [| L.const_int i32_t cols; L.const_int i32_t rows |] "matrix_init" builder
              in
              ignore(List.map (fun v -> L.build_call store_matrix_f [| m ; v |] "store_val" builder) contents'); m

      | SUnop(op, ((t, _) as e)) ->
          let e' = expr builder e in 
          (match op with
	          A.Neg when t = A.Num -> L.build_fneg 
	        | A.Neg                  -> L.build_neg
          | A.Not                  -> L.build_not) e' "tmp" builder
      (*In fashion of microc have typechecking for *)
      | SEmpty -> L.const_int i32_t 0
      | SBinop (e1, op, e2) -> 
        let (_typ, _ ) = e1 in (*get type of first expression, semant should have checked that both types of e1 and e2 should be same*)
        let e1' = expr builder e1 in
        let e2' = expr builder e2 in (match _typ with 
        (*this looks much cleaner*)
        (*binary bool operations!*)
          A.Bool -> (match op with 
            A.And     -> L.build_and
            | A.Or      -> L.build_or 
            | A.Equal   -> L.build_icmp L.Icmp.Eq 
            | A.Neq     -> L.build_icmp L.Icmp.Ne
            | _         -> raise (Failure "internal error: semant should have rejected and/or on float")
          ) e1' e2' "tmp" builder
          (* num operations*)
          | A.Int | A.String | A.Void | A.Num -> (match op with 
                A.Add     -> L.build_fadd
              | A.Sub     -> L.build_fsub
              | A.Mult    -> L.build_fmul
              | A.Div     -> L.build_fdiv 
              | A.Mod     -> L.build_frem
              (* | A.Exp     -> L.build_call exp_func [| e1'; e2'|] "exp" builder double check this *)
              | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
              | A.Neq     -> L.build_fcmp L.Fcmp.One
              | A.Less    -> L.build_fcmp L.Fcmp.Olt
              | A.Leq     -> L.build_fcmp L.Fcmp.Ole
              | A.Great -> L.build_fcmp L.Fcmp.Ogt
              | A.Geq     -> L.build_fcmp L.Fcmp.Oge
              | A.And 
              | A.Or      -> raise (Failure "internal error: semant should have rejected and/or on float")
              ) e1' e2' "tmp" builder
        | A.Xirtam -> raise (Failure "cannot use binop on matrices")
      )


        (*binary bool operations*)
        (*original is below*)
        (* 
        (match op with
          A.Add     -> L.build_add
        | A.Sub     -> L.build_sub
        | A.Mult    -> L.build_mul
        | A.Div     -> L.build_sdiv
        | A.Mod     -> L.build_srem  e1' e2' "tmp" builder (* mod for other stuff*)
        | A.And     -> L.build_and
        | A.Or      -> L.build_or
        | A.Equal   -> L.build_icmp L.Icmp.Eq
        | A.Neq     -> L.build_icmp L.Icmp.Ne
        | A.Less    -> L.build_icmp L.Icmp.Slt
        | A.Leq     -> L.build_icmp L.Icmp.Sle
        | A.Great -> L.build_icmp L.Icmp.Sgt
        | A.Geq     -> L.build_icmp L.Icmp.Sge
        	  ) e1' e2' "tmp" builder
        *)

      | SAssign (s, e) -> let e' = expr builder e in
                          ignore(L.build_store e' (lookup s) builder); e'
      | SCall ("printn", [e]) -> 
	  L.build_call printf_func [| float_format_str ; (expr builder e) |]
	    "printf" builder
      | SCall ("printm", [e]) ->
        L.build_call printMatrix_f [| (expr builder e) |] "printm" builder
      | SCall ("matmult", [e1; e2]) ->
        L.build_call mult_matrix_f [| (expr builder e1); (expr builder e2) |] "matmult" builder
      | SCall ("matadd", [e1; e2]) ->
        L.build_call add_matrix_f [| (expr builder e1); (expr builder e2) |] "matadd" builder
        (* acces and get *)
      | SCall ("matget", [e1; e2; e3;]) ->
        L.build_call get_matrix_el_f [| (expr builder e1); (expr builder e2);(expr builder e3) |] "matget" builder
      | SCall ("matset", [e1; e2; e3; e4;]) ->
        L.build_call set_matrix_el_f [| (expr builder e1); (expr builder e2);(expr builder e3);(expr builder e4) |] "matset" builder


      | SCall (f, args) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 let llargs = List.rev (List.map (expr builder) (List.rev args)) in
	 let result = (match fdecl.styp with 
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list llargs) result builder
    in
    (* *)
 
    (* LLVM insists each basic block end with exactly one "terminator" 
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
	   Some _ -> ()
      | None -> ignore (instr builder) in
(* statements and control flow*)
 let rec stmt builder = function
  SBlock sl -> List.fold_left stmt builder sl
      | SExpr e -> ignore(expr builder e); builder 
      | SReturn e -> ignore(match fdecl.styp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder 
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder e) builder );
                     builder
      (* are we adding continue?*)
      | SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
   let merge_bb = L.append_block context "merge" the_function in
         let build_br_merge = L.build_br merge_bb in (* partial function *)

   let then_bb = L.append_block context "then" the_function in
   add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
     build_br_merge;

   let else_bb = L.append_block context "else" the_function in
   add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
     build_br_merge;

   ignore(L.build_cond_br bool_val then_bb else_bb builder);
   L.builder_at_end context merge_bb

      | SWhile (predicate, body) ->
    let pred_bb = L.append_block context "while" the_function in
    ignore(L.build_br pred_bb builder);

    let body_bb = L.append_block context "while_body" the_function in
    add_terminal (stmt (L.builder_at_end context body_bb) body)
      (L.build_br pred_bb);

    let pred_builder = L.builder_at_end context pred_bb in
    let bool_val = expr pred_builder predicate in

    let merge_bb = L.append_block context "merge" the_function in
    ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
    L.builder_at_end context merge_bb

      (* Implement for loops as while loops *)
      | SFor (e1, e2, e3, body) -> stmt builder
      ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (SBlock fdecl.sf_statements) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      (* | A.Int -> L.build_ret (L.const_int i32_t 0) *)
      | A.Num -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module


