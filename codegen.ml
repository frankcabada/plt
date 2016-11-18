module L = Llvm
module A = Ast
module S = Sast

module StringMap = Map.Make(String)

let translate(globals, functions) =
  let context = L.global_context() in
  let the_module = L.create_module context "CMAT"

  and i32_t   = L.i32_type context
  and i8_t    = L.i8_type context
  and void_t  = L.void_type context in

  let ltype_of_typ = function
      A.Int  -> i32_t
    | A.Void -> void_t in

  let ltype_of_datatype = function
    A.Datatype(p) -> ltype_of_typ p in

  let printf_t =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func =
    L.declare_function "printf" printf_t the_module in

  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types = Array.of_list
        (List.map (function A.Formal(t,s) -> ltype_of_datatype t) fdecl.A.formals)
      in let ftype =
        L.function_type (ltype_of_datatype fdecl.A.return_type) formal_types in
        StringMap.add name (L.define_function name ftype the_module, fdecl) m in
        List.fold_left function_decl StringMap.empty functions in

  let build_function_body fdecl =

    let (the_function, _) =
      StringMap.find fdecl.A.fname function_decls in

    let builder = (* Create an instruction builder *)
      L.builder_at_end context (L.entry_block the_function) in

  let local_vars =
    let add_formal m (t, n) p = L.set_value_name n p;
      let local = L.build_alloca (ltype_of_datatype t) n builder in
      ignore (L.build_store p local builder);
      StringMap.add n local m in

    let add_local m (t, n) =
      let local_var = L.build_alloca (ltype_of_typ t) n builder
      in StringMap.add n local_var m in

    let formals = List.fold_left2 add_formal StringMap.empty
      fdecl.A.formals (Array.to_list (L.params the_function)) in
    List.fold_left add_local formals fdecl.A.locals in

    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    let rec expr builder = function
        A.Int_lit i     -> L.const_int i32_t i
      | A.Float_lit i   -> L.const_int i64_t i
      | A.Bool_lit b    -> L.const_int i1_t (if b then 1 else 0)
      | A.Noexpr        -> L.const_int i32_t 0
      | A.Id s          -> L.build_load (lookup s) s builder
      | A.Assign (s, e) -> let e' = expr builder e in
              ignore (L.build_store e' (lookup s) builder); e'
      | A.Unop(op, e)   ->
          let e' = expr builder e in match op with
            A.Neg   -> L.build_neg
          | A.Not   -> L.build_not e' "tmp" builder;
      | A.Call ("print", [e]) | A.Call ("printb", [e]) ->
          L.build_call printf_func
            [| int_format_str ; (expr builder e) |]
            "printf" builder
      | A.Call (f, act) ->
          let (fdef, fdecl) = StringMap.find f function_decls in
          let actuals = List.rev (List.map (expr builder) (List.rev act)) in
          let result = (match fdecl.A.return_type with A.Void -> ""
                                                       | _ -> f ^ "_result") in
          L.build_call fdef (Array.of_list actuals) result builder in

    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (f builder) in
    let rec stmt builder = function
        A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore (match fdecl.A.return_type with
          A.Void -> L.build_ret_void builder
        | _ -> L.build_ret (expr builder e) builder); builder in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.return_type with
        A.Voi -> L.build_ret_void
        | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
    in
    List.iter build_function_body functions;
    the_module
