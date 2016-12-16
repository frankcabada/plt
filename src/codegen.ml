open Llvm
open Ast
open Sast
open Semant
module L = Llvm
module A = Ast
module S = Sast

module StringMap = Map.Make(String)

let translate(globals, functions) =

    let context = L.global_context() in
    let the_module = L.create_module context "CMAT"

    and i32_t     = L.i32_type context
    and i1_t      = L.i1_type context
    and i8_t      = L.i8_type context
    and float_t   = L.double_type context
    and void_t    = L.void_type context
    and array_t   = L.array_type
    and pointer_t = L.pointer_type
    in

    let ltype_of_typ = function
          A.Int     -> i32_t
        | A.Float   -> float_t
        | A.Bool    -> i1_t
        | A.Void    -> void_t
        | A.String  -> pointer_t i8_t
        | A.Vector(typ, size) ->
            let size' = match size with Int_lit(s) -> s | _ -> raise(Exceptions.InvalidVectorDimension) in
            (match typ with
                A.Int      ->  array_t i32_t size'
                | A.Float  -> array_t float_t size'
                | _ -> raise(Exceptions.UnsupportedVectorType))
        | A.Matrix(typ, rows, cols) ->
            let rows' = match rows with Int_lit(s) -> s | _ -> raise(Exceptions.InvalidMatrixDimension) in
            let cols' = match cols with Int_lit(s) -> s | _ -> raise(Exceptions.InvalidMatrixDimension) in
            (match typ with
                A.Int      -> array_t (array_t i32_t cols') rows'
                | A.Float  -> array_t (array_t float_t cols') rows'
                | _ -> raise(Exceptions.UnsupportedMatrixType))
    in

    let ltype_of_datatype = function
        A.Datatype(p) -> ltype_of_typ p
    in

    let global_vars =
        let global_var m (t,n) =
            let init = L.const_int (ltype_of_datatype t) 0 in
                StringMap.add n (L.define_global n init the_module) m in
                    List.fold_left global_var StringMap.empty globals
    in

    let printf_t =
        L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
    in
    let printf_func =
        L.declare_function "printf" printf_t the_module
    in

    let function_decls =
        let function_decl m fdecl =
            let name = fdecl.S.sfname
            and formal_types = Array.of_list
                (List.map (function A.Formal(t,s) -> ltype_of_datatype t) fdecl.S.sformals) in
            let ftype =
                L.function_type (ltype_of_datatype fdecl.S.sreturn_type) formal_types in
                StringMap.add name (L.define_function name ftype the_module, fdecl) m in
        List.fold_left function_decl StringMap.empty functions
    in

    let build_function_body fdecl =

        let (the_function, _) = StringMap.find fdecl.S.sfname function_decls in

        (* Create an instruction builder *)
        let builder = L.builder_at_end context (L.entry_block the_function) in

        let int_format_str = L.build_global_stringptr "%d " "fmt" builder
        and float_format_str = L.build_global_stringptr "%f " "fmt" builder
        in

        let local_vars =
            let add_formal m (t, n) p = L.set_value_name n p;
                let local = L.build_alloca (ltype_of_datatype t) n builder in
                ignore (L.build_store p local builder);
            StringMap.add n local m
        in

        let add_local m (t, n) =
            let local_var = L.build_alloca (ltype_of_datatype t) n builder
            in StringMap.add n local_var m
        in

        let formals = List.fold_left2 add_formal StringMap.empty
            (List.map (function A.Formal(t,n) -> (t,n)) fdecl.S.sformals) (Array.to_list (L.params the_function)) in
            List.fold_left add_local formals (List.map (function A.Local(t,n) -> (t,n)) fdecl.S.slocals)
        in

        let lookup n = try StringMap.find n local_vars
            with Not_found -> StringMap.find n global_vars
        in

        let build_matrix_access s i1 i2 i3 builder isAssign =
            if isAssign
                then L.build_gep (lookup s) [| i1; i2; i3 |] s builder
                else L.build_load (L.build_gep (lookup s) [| i1; i2; i3 |] s builder) s builder
        in

        let build_vector_access s i1 i2 builder isAssign =
            if isAssign
                then L.build_gep (lookup s) [| i1; i2 |] s builder
                else L.build_load (L.build_gep (lookup s) [| i1; i2 |] s builder) s builder
        in

        let rec expr builder = function
            S.SNum_lit(SInt_lit(n))     -> L.const_int i32_t n
            | S.SNum_lit(SFloat_lit(f)) -> L.const_float float_t f
            | S.SBool_lit b             -> L.const_int i1_t (if b then 1 else 0)
            | S.SString_lit s           -> L.build_global_stringptr s "tmp" builder
            | S.SNoexpr                 -> L.const_int i32_t 0
            | S.SId (s, d)              -> L.build_load (lookup s) s builder
            | S.SAssign (se1, se2, d)   ->
                let se1' =
                    (match se1 with
                        S.SId(s,_) -> (lookup s)
                        | S.SMatrix_access(s, i1, j1, d) ->
                            let i = expr builder i1 and j = expr builder j1 in
                                build_matrix_access s (L.const_int i32_t 0) i j builder true
                        | S.SVector_access(s, i1, d) ->
                            let i = expr builder i1 in
                                build_vector_access s (L.const_int i32_t 0) i builder true
                        | _ -> raise(Exceptions.AssignLHSMustBeAssignable))
                and se2' = expr builder se2 in
                ignore (L.build_store se2' se1' builder); se2'
            | S.SBinop (e1, op, e2, d)  ->
                let type1 = Semant.get_type_from_sexpr e1 in
                let type2 = Semant.get_type_from_sexpr e2 in
                let e1 = expr builder e1
                and e2 = expr builder e2 in

                let int_bops op e1' e2' =
                    match op with
                        A.Add     -> L.build_add e1' e2' "tmp" builder
                        | A.Sub     -> L.build_sub e1' e2' "tmp" builder
                        | A.Mult    -> L.build_mul e1' e2' "tmp" builder
                        | A.Div     -> L.build_sdiv e1' e2' "tmp" builder
                        | A.Equal   -> L.build_icmp L.Icmp.Eq e1' e2' "tmp" builder
                        | A.Neq     -> L.build_icmp L.Icmp.Ne e1' e2' "tmp" builder
                        | A.Less    -> L.build_icmp L.Icmp.Slt e1' e2' "tmp" builder
                        | A.Leq     -> L.build_icmp L.Icmp.Sle e1' e2' "tmp" builder
                        | A.Greater -> L.build_icmp L.Icmp.Sgt e1' e2' "tmp" builder
                        | A.Geq     -> L.build_icmp L.Icmp.Sge e1' e2' "tmp" builder
                        | _         -> raise(Exceptions.IllegalIntBinop)
                in

                let float_bops op e1' e2' =
                    match op with
                        A.Add       -> L.build_fadd e1' e2' "tmp" builder
                        | A.Sub       -> L.build_fsub e1' e2' "tmp" builder
                        | A.Mult      -> L.build_fmul e1' e2' "tmp" builder
                        | A.Div       -> L.build_fdiv e1' e2' "tmp" builder
                        | A.Equal     -> L.build_fcmp L.Fcmp.Oeq e1' e2' "tmp" builder
                        | A.Neq       -> L.build_fcmp L.Fcmp.One e1' e2' "tmp" builder
                        | A.Less      -> L.build_fcmp L.Fcmp.Olt e1' e2' "tmp" builder
                        | A.Leq       -> L.build_fcmp L.Fcmp.Ole e1' e2' "tmp" builder
                        | A.Greater   -> L.build_fcmp L.Fcmp.Ogt e1' e2' "tmp" builder
                        | A.Geq       -> L.build_fcmp L.Fcmp.Oge e1' e2' "tmp" builder
                        | _           -> raise(Exceptions.IllegalFloatBinop)
                in

                let bool_bops op e1' e2' =
                    match op with
                        | A.And   -> L.build_and e1' e2' "tmp" builder
                        | A.Or    -> L.build_or e1' e2' "tmp" builder
                        | _       -> raise(Exceptions.IllegalBoolBinop)
                in

                (*let vector_bops op e1' e2' =
                    match op with
                        A.Add       -> L.build_add e1' e2' "tmp" builder
                        | A.Sub     -> L.build_sub e1' e2' "tmp" builder
                        | A.Mult    -> L.build_mul e1' e2' "tmp" builder
                        | _         -> raise(Exceptions.IllegalVectorBinop)
                in

                let matrix_bops op e1' e2' =
                    match op with
                        A.Add       -> L.build_add e1' e2' "tmp" builder
                        | A.Sub     -> L.build_sub e1' e2' "tmp" builder
                        | A.Mult    -> L.build_mul e1' e2' "tmp" builder
                        | _         -> raise(Exceptions.IllegalMatrixBinop)
                in*)

                let cast lhs rhs lhsType rhsType =
                    match (lhsType, rhsType) with
                        (Datatype(Int), Datatype(Int))    -> (lhs, rhs), Datatype(Int)
                        | (Datatype(Float), Datatype(Float))->  (lhs, rhs), Datatype(Float)
                        | (Datatype(Bool), Datatype(Bool))  ->  (lhs, rhs), Datatype(Bool)
                        | (Datatype(Int), Datatype(Float))  ->   (build_sitofp lhs float_t "tmp" builder, rhs), Datatype(Float)
                        | (Datatype(Float), Datatype(Int))  ->   (lhs, build_sitofp rhs float_t "tmp" builder), Datatype(Float)
                        | _                                 -> raise(Exceptions.IllegalCast)
                in

                let (e1, e2), d = cast e1 e2 type1 type2 in

                let check_binop_type d =
                    match d with
                        Datatype(Int)               -> int_bops op e1 e2
                        | Datatype(Float)           -> float_bops op e1 e2
                        | Datatype(Bool)            -> bool_bops op e1 e2
                        (*| Datatype(Vector(_,_))     -> vector_bops op e1 e2
                        | Datatype(Matrix(_,_,_))   -> matrix_bops op e1 e2*)
                        | _                         -> raise(Exceptions.UnsupportedBinopType)
                in

                check_binop_type d
            | S.SUnop(op, e, d)         ->
                let e' = expr builder e in
                let int_unops op =
                    (match op with
                        A.Neg     -> L.build_neg e' "tmp" builder
                        | A.Not   -> L.build_not e' "tmp" builder
                        | A.Inc   -> L.build_store (L.build_add e' (L.const_int i32_t 1) "tmp" builder) (lookup (match e with S.SId(s, d) -> s | _->raise(Exceptions.IncMustBeCalledOnID))) builder
                        | A.Dec   -> L.build_store (L.build_sub e' (L.const_int i32_t 1) "tmp" builder) (lookup (match e with S.SId(s, d) -> s | _->raise(Exceptions.DecMustBeCalledOnID))) builder)
                in
                let float_unops op =
                    match op with
                        A.Neg   -> L.build_fneg e' "tmp" builder
                        | _     -> raise(Exceptions.IllegalFloatUnop)
                in
                let bool_unops op =
                    match op with
                        A.Not   -> L.build_not e' "tmp" builder
                        | _       -> raise(Exceptions.IllegalBoolUnop)
                in
                let check_unop_type d =
                    match d with
                        Datatype(Int)   -> int_unops op
                        | Datatype(Float) -> float_unops op
                        | Datatype(Bool)  -> bool_unops op
                        | _               -> raise(Exceptions.InvalidUnopType)
                in
                check_unop_type d
            | S.SRows(r)                -> L.const_int i32_t r
            | S.SCols(c)                -> L.const_int i32_t c
            | S.SLen(l)                 -> L.const_int i32_t l
            | S.SCall ("print_string", [e], d) ->
                let get_string = function S.SString_lit s -> s | _ -> "" in
                let s_ptr = L.build_global_stringptr ((get_string e) ^ "\n") ".str" builder in
                    L.build_call printf_func [| s_ptr |] "printf" builder
            | S.SCall ("print_int", [e], d) ->
                L.build_call printf_func [| int_format_str ; (expr builder e) |] "printf" builder
            | S.SCall ("print_float", [e], d) ->
                L.build_call printf_func [| float_format_str ; (expr builder e) |] "printf" builder
            | S.SCall (f, act, d) ->
                let (fdef, fdecl) = StringMap.find f function_decls in
                let actuals = List.rev (List.map (expr builder) (List.rev act)) in
                let result =
                    (match fdecl.S.sreturn_type with
                        A.Datatype(A.Void) -> ""
                        | _ -> f ^ "_result") in
                L.build_call fdef (Array.of_list actuals) result builder
            | S.SNull                   -> L.const_null i32_t
            | S.SMatrix_access (s, se1, se2, d) ->
                let i = expr builder se1 and j = expr builder se2 in
                    (build_matrix_access s (L.const_int i32_t 0) i j builder false)
            | S.SVector_access (s, se, d) ->
                let i = expr builder se in
                    (build_vector_access s (L.const_int i32_t 0) i builder false)
            | S.SMatrix_lit (sll, d) ->
                (match d with
                    A.Datatype(A.Float) ->
                        let realOrder       = List.map List.rev sll in
                        let i64Lists        = List.map (List.map (expr builder)) realOrder in
                        let listOfArrays    = List.map Array.of_list i64Lists in
                        let i64ListOfArrays = List.rev (List.map (L.const_array float_t) listOfArrays) in
                        let arrayOfArrays   = Array.of_list i64ListOfArrays in
                            L.const_array (array_t float_t (List.length (List.hd sll))) arrayOfArrays
                    | A.Datatype(A.Int)  ->
                        let realOrder       = List.map List.rev sll in
                        let i32Lists        = List.map (List.map (expr builder)) realOrder in
                        let listOfArrays    = List.map Array.of_list i32Lists in
                        let i32ListOfArrays = List.rev (List.map (L.const_array i32_t) listOfArrays) in
                        let arrayOfArrays   = Array.of_list i32ListOfArrays in
                            L.const_array (array_t i32_t (List.length (List.hd sll))) arrayOfArrays
                    | _ -> raise(Exceptions.UnsupportedMatrixType))
            (*| S.SMatrix_row (_, _, _)
            | S.SMatrix_col (_, _, _)*)
        in

        let add_terminal builder f =
            match L.block_terminator (L.insertion_block builder) with
                Some _ -> ()
                | None -> ignore (f builder)
        in

        let rec stmt builder = function
            S.SBlock sl -> List.fold_left stmt builder sl
            | S.SExpr e -> ignore (expr builder e); builder
            | S.SReturn e ->
                ignore(match fdecl.S.sreturn_type with
                    A.Datatype(A.Void)  -> L.build_ret_void builder
                    | _                 -> L.build_ret (expr builder e) builder); builder
            | S.SIf (predicate, then_stmt, else_stmt) ->
                let bool_val = expr builder predicate in
                let merge_bb = L.append_block context
                    "merge" the_function in
                let then_bb = L.append_block context
                    "then" the_function in
                add_terminal
                    (stmt (L.builder_at_end context then_bb) then_stmt)
                    (L.build_br merge_bb);
                let else_bb = L.append_block context
                    "else" the_function in
                add_terminal
                    (stmt (L.builder_at_end context else_bb) else_stmt)
                    (L.build_br merge_bb);
                ignore (L.build_cond_br bool_val then_bb else_bb builder);
                L.builder_at_end context merge_bb
            | S.SWhile (predicate, body) ->
                let pred_bb = L.append_block context
                    "while" the_function in
                ignore (L.build_br pred_bb builder);
                let body_bb = L.append_block context
                    "while_body" the_function in
                add_terminal (stmt (L.builder_at_end context body_bb) body)
                (L.build_br pred_bb);
                let pred_builder = L.builder_at_end context pred_bb in
                let bool_val = expr pred_builder predicate in
                let merge_bb = L.append_block context
                    "merge" the_function in
                ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
                L.builder_at_end context merge_bb
            | S.SFor (e1, e2, e3, body) -> stmt builder
                (S.SBlock [S.SExpr e1 ;
                    S.SWhile (e2, S.SBlock [body ;
                        S.SExpr e3]) ])
        in

        (* Build the code for each statement in the function *)
        let builder = stmt builder (S.SBlock fdecl.S.sbody) in

        (* Add a return if the last block falls off the end *)
        add_terminal builder
            (match fdecl.S.sreturn_type with
                A.Datatype(A.Void) -> L.build_ret_void;
                | t -> L.build_ret (L.const_int (ltype_of_datatype t) 0))
    in
    List.iter build_function_body functions;

    the_module (*returned as a module to whatever called this*)
