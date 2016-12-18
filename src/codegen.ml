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

        let int_format_str = L.build_global_stringptr "%d\t" "fmt" builder
        and float_format_str = L.build_global_stringptr "%f\t" "fmt" builder
        and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder
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

        let build_matrix_access access_i access_j s i1 i2 i3 builder isAssign =
            let rows = L.array_length (L.type_of (L.build_load (L.build_gep (lookup s) [| L.const_int i32_t 0 |] s builder) s builder)) in
            let cols = L.array_length (L.type_of (L.build_load (L.build_gep (lookup s) [| L.const_int i32_t 0; L.const_int i32_t 0 |] s builder) s builder)) in
            if ((rows < access_i) && (cols > access_j)) then raise(Exceptions.MatrixOutOfBoundsAccess(""));
            if isAssign
                then L.build_gep (lookup s) [| i1; i2; i3 |] s builder
                else L.build_load (L.build_gep (lookup s) [| i1; i2; i3 |] s builder) s builder
        in

        let build_vector_access access_i s i1 i2 builder isAssign =
            let len = L.array_length (L.type_of (L.build_load (L.build_gep (lookup s) [| L.const_int i32_t 0 |] s builder) s builder)) in
            if (len < access_i) then raise(Exceptions.VectorOutOfBoundsAccess(""));
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
                            let access_i = (match i1 with S.SNum_lit(SInt_lit(n)) -> n | _ -> -1)
                            and access_j = (match j1 with S.SNum_lit(SInt_lit(n)) -> n | _ -> -1) in
                                build_matrix_access access_i access_j s (L.const_int i32_t 0) i j builder true
                        | S.SVector_access(s, i1, d) ->
                            let i = expr builder i1 in
                            let access_i = (match i1 with S.SNum_lit(SInt_lit(n)) -> n | _ -> -1) in
                                build_vector_access access_i s (L.const_int i32_t 0) i builder true
                        | _ -> raise(Exceptions.AssignLHSMustBeAssignable))
                and se2' = expr builder se2 in
                ignore (L.build_store se2' se1' builder); se2'
            | S.SBinop (e1, op, e2, d)  ->
                let type1 = Semant.get_type_from_sexpr e1 in
                let type2 = Semant.get_type_from_sexpr e2 in
                let e1' = expr builder e1
                and e2' = expr builder e2 in

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
                        | A.And     -> L.build_and e1' e2' "tmp" builder
                        | A.Or      -> L.build_or e1' e2' "tmp" builder
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

                let vector_bops iorf n_i n op e1 e2 =
                    let lhs_str = (match e1 with SId(s,_) -> s | _ -> "") in
                    let rhs_str = (match e2 with SId(s,_) -> s | _ -> "") in
                    match iorf with
                        "int" ->
                            (match op with
                                A.Add       ->
                                    let tmp_v = L.build_alloca (array_t i32_t n_i) "tmpvec" builder in
                                    for i=0 to n_i do
                                            let v1 = build_vector_access n_i lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                                            let v2 = build_vector_access n_i rhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                                            let add_res = L.build_add v1 v2 "tmp" builder in
                                            let ld = L.build_gep tmp_v [| L.const_int i32_t 0; L.const_int i32_t i |] "tmpvec" builder in
                                            ignore(build_store add_res ld builder);
                                    done;
                                    L.build_load (L.build_gep tmp_v [| L.const_int i32_t 0 |] "tmpvec" builder) "tmpvec" builder
                                | A.Sub     ->
                                    let tmp_v = L.build_alloca (array_t i32_t n_i) "tmpvec" builder in
                                    for i=0 to n_i do
                                            let v1 = build_vector_access n_i lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                                            let v2 = build_vector_access n_i rhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                                            let add_res = L.build_sub v1 v2 "tmp" builder in
                                            let ld = L.build_gep tmp_v [| L.const_int i32_t 0; L.const_int i32_t i |] "tmpvec" builder in
                                            ignore(build_store add_res ld builder);
                                    done;
                                    L.build_load (L.build_gep tmp_v [| L.const_int i32_t 0 |] "tmpvec" builder) "tmpvec" builder
                                | A.Mult    ->
                                    let first_typ = Semant.get_type_from_sexpr e1 in
                                    let tmp_v = L.build_alloca (array_t i32_t n_i) "tmpvec" builder in
                                    (match first_typ with
                                        Datatype(Int) ->
                                            for i=0 to n_i do
                                                let v2 = build_vector_access n_i rhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                                                let add_res = L.build_mul (build_load (lookup lhs_str) "tmp" builder) v2 "tmp" builder in
                                                let ld = L.build_gep tmp_v [| L.const_int i32_t 0; L.const_int i32_t i |] "tmpvec" builder in
                                                ignore(build_store add_res ld builder);
                                            done;
                                            L.build_load (L.build_gep tmp_v [| L.const_int i32_t 0 |] "tmpvec" builder) "tmpvec" builder
                                        | Datatype(Matrix(Int,r1,c1)) -> L.const_int i32_t 0
                                        | _ -> L.const_int i32_t 0)
                                | _         -> raise(Exceptions.IllegalVectorBinop))
                        | "float" ->
                            (match op with
                                A.Add       ->
                                    let lhs_str = (match e1 with SId(s,_) -> s | _ -> "") in
                                    let rhs_str = (match e2 with SId(s,_) -> s | _ -> "") in
                                    let tmp_v = L.build_alloca (array_t float_t n_i) "tmpvec" builder in
                                    for i=0 to n_i do
                                            let v1 = build_vector_access n_i lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                                            let v2 = build_vector_access n_i rhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                                            let add_res = L.build_fadd v1 v2 "tmp" builder in
                                            let ld = L.build_gep tmp_v [| L.const_int i32_t 0; L.const_int i32_t i |] "tmpvec" builder in
                                            ignore(build_store add_res ld builder);
                                    done;
                                    L.build_load (L.build_gep tmp_v [| L.const_int i32_t 0 |] "tmpvec" builder) "tmpvec" builder
                                | A.Sub     ->
                                    let lhs_str = (match e1 with SId(s,_) -> s | _ -> "") in
                                    let rhs_str = (match e2 with SId(s,_) -> s | _ -> "") in
                                    let tmp_v = L.build_alloca (array_t float_t n_i) "tmpvec" builder in
                                    for i=0 to n_i do
                                            let v1 = build_vector_access n_i lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                                            let v2 = build_vector_access n_i rhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                                            let add_res = L.build_fsub v1 v2 "tmp" builder in
                                            let ld = L.build_gep tmp_v [| L.const_int i32_t 0; L.const_int i32_t i |] "tmpvec" builder in
                                            ignore(build_store add_res ld builder);
                                    done;
                                    L.build_load (L.build_gep tmp_v [| L.const_int i32_t 0 |] "tmpvec" builder) "tmpvec" builder
                                | A.Mult    ->
                                    let first_typ = Semant.get_type_from_sexpr e1 in
                                    let tmp_v = L.build_alloca (array_t float_t n_i) "tmpvec" builder in
                                    (match first_typ with
                                        Datatype(Float) ->
                                            for i=0 to n_i do
                                                let v2 = build_vector_access n_i rhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                                                let add_res = L.build_fmul (build_load (lookup lhs_str) "tmp" builder) v2 "tmp" builder in
                                                let ld = L.build_gep tmp_v [| L.const_int i32_t 0; L.const_int i32_t i |] "tmpvec" builder in
                                                ignore(build_store add_res ld builder);
                                            done;
                                            L.build_load (L.build_gep tmp_v [| L.const_int i32_t 0 |] "tmpvec" builder) "tmpvec" builder
                                        | Datatype(Matrix(Int,r1,c1)) -> L.const_int i32_t 0
                                        | _ -> L.const_int i32_t 0)
                                | _         -> raise(Exceptions.IllegalVectorBinop))
                        | _ -> L.const_int i32_t 0
                in

                let matrix_bops iorf r_i c_i r c op e1 e2 =
                    let lhs_str = (match e1 with SId(s,_) -> s | _ -> "") in
                    let rhs_str = (match e2 with SId(s,_) -> s | _ -> "") in
                    match iorf with
                        "int" ->
                            (match op with
                                A.Add       ->
                                    let tmp_m = L.build_alloca (array_t (array_t i32_t c_i) r_i) "tmpmat" builder in
                                    for i=0 to (r_i-1) do
                                        for j=0 to (c_i-1) do
                                            let m1 = build_matrix_access r_i c_i lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                                            let m2 = build_matrix_access r_i c_i rhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                                            let add_res = L.build_add m1 m2 "tmp" builder in
                                            let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                                            ignore(build_store add_res ld builder);
                                        done
                                    done;
                                    L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
                                | A.Sub     ->
                                    let tmp_m = L.build_alloca (array_t (array_t i32_t c_i) r_i) "tmpmat" builder in
                                    for i=0 to (r_i-1) do
                                        for j=0 to (c_i-1) do
                                            let m1 = build_matrix_access r_i c_i lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                                            let m2 = build_matrix_access r_i c_i rhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                                            let add_res = L.build_sub m1 m2 "tmp" builder in
                                            let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                                            ignore(build_store add_res ld builder);
                                        done
                                    done;
                                    L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
                                | A.Mult    ->
                                    let first_typ = Semant.get_type_from_sexpr e1 in
                                    let tmp_m = L.build_alloca (array_t (array_t i32_t c_i) r_i) "tmpmat" builder in
                                    (match first_typ with
                                        Datatype(Int) ->
                                            for i=0 to (r_i-1) do
                                                for j=0 to (c_i-1) do
                                                    let m2 = build_matrix_access r_i c_i rhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                                                    let add_res = L.build_mul (build_load (lookup lhs_str) "tmp" builder) m2 "tmp" builder in
                                                    let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                                                    ignore(build_store add_res ld builder);
                                                done
                                            done;
                                            L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
                                        | Datatype(Matrix(Int,r1,c1)) ->
                                            let tmp_s = L.build_alloca i32_t "tmpsum" builder in
                                            let c1_i = (match c1 with Int_lit(n) -> n | _ -> -1) in
                                            ignore(L.build_store (L.const_int i32_t 0) tmp_s builder);
                                            for i=0 to (r_i-1) do
                                                for j=0 to (c_i-1) do
                                                    ignore(L.build_store (L.const_int i32_t 0) tmp_s builder);
                                                    for k=0 to (c1_i-1) do
                                                        let m1 = build_matrix_access r_i c1_i lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t k) builder false in
                                                        let m2 = build_matrix_access c1_i c_i rhs_str (L.const_int i32_t 0) (L.const_int i32_t k) (L.const_int i32_t j) builder false in
                                                        let mult_res = L.build_mul m1 m2 "tmp" builder in
                                                        ignore(L.build_store (L.build_add mult_res (L.build_load tmp_s "addtmp" builder) "tmp" builder) tmp_s builder);
                                                    done;
                                                    let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                                                    ignore(build_store (L.build_load tmp_s "restmp" builder) ld builder);
                                                done
                                            done;
                                            L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
                                        | _ -> L.const_int i32_t 0)
                                | _         -> raise(Exceptions.IllegalMatrixBinop))
                        | "float" ->
                            (match op with
                                A.Add       ->
                                    let tmp_m = L.build_alloca (array_t (array_t float_t c_i) r_i) "tmpmat" builder in
                                    for i=0 to (r_i-1) do
                                        for j=0 to (c_i-1) do
                                            let m1 = build_matrix_access r_i c_i lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                                            let m2 = build_matrix_access r_i c_i rhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                                            let add_res = L.build_fadd m1 m2 "tmp" builder in
                                            let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                                            ignore(build_store add_res ld builder);
                                        done
                                    done;
                                    L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
                                | A.Sub     ->
                                    let tmp_m = L.build_alloca (array_t (array_t float_t c_i) r_i) "tmpmat" builder in
                                    for i=0 to (r_i-1) do
                                        for j=0 to (c_i-1) do
                                            let m1 = build_matrix_access r_i c_i lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                                            let m2 = build_matrix_access r_i c_i rhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                                            let add_res = L.build_fsub m1 m2 "tmp" builder in
                                            let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                                            ignore(build_store add_res ld builder);
                                        done
                                    done;
                                    L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
                                | A.Mult    ->
                                    let first_typ = Semant.get_type_from_sexpr e1 in
                                    let tmp_m = L.build_alloca (array_t (array_t float_t c_i) r_i) "tmpmat" builder in
                                    (match first_typ with
                                        Datatype(Float) ->
                                            for i=0 to (r_i-1) do
                                                for j=0 to (c_i-1) do
                                                    let m2 = build_matrix_access r_i c_i rhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                                                    let add_res = L.build_fmul (build_load (lookup lhs_str) "tmp" builder) m2 "tmp" builder in
                                                    let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                                                    ignore(build_store add_res ld builder);
                                                done
                                            done;
                                            L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
                                        | Datatype(Matrix(Float,r1,c1)) ->
                                            let tmp_s = L.build_alloca float_t "tmpsum" builder in
                                            let c1_i = (match c1 with Int_lit(n) -> n | _ -> -1) in
                                            ignore(L.build_store (L.const_float float_t 0.0) tmp_s builder);
                                            for i=0 to (r_i-1) do
                                                for j=0 to (c_i-1) do
                                                    ignore(L.build_store (L.const_float float_t 0.0) tmp_s builder);
                                                    for k=0 to (c1_i-1) do
                                                        let m1 = build_matrix_access r_i c1_i lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t k) builder false in
                                                        let m2 = build_matrix_access c1_i c_i rhs_str (L.const_int i32_t 0) (L.const_int i32_t k) (L.const_int i32_t j) builder false in
                                                        let mult_res = L.build_fmul m1 m2 "tmp" builder in
                                                        ignore(L.build_store (L.build_fadd mult_res (L.build_load tmp_s "addtmp" builder) "tmp" builder) tmp_s builder);
                                                    done;
                                                    let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                                                    ignore(build_store (L.build_load tmp_s "restmp" builder) ld builder);
                                                done
                                            done;
                                            L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
                                        | _ -> L.const_int i32_t 0)
                                | _         -> raise(Exceptions.IllegalMatrixBinop))
                        | _ -> L.const_int i32_t 0
                in

                let cast lhs rhs lhsType rhsType =
                    match (lhsType, rhsType) with
                        (Datatype(Int), Datatype(Int))          -> (lhs, rhs), Datatype(Int)
                        | (Datatype(Float), Datatype(Float))    ->  (lhs, rhs), Datatype(Float)
                        | (Datatype(Bool), Datatype(Bool))      ->  (lhs, rhs), Datatype(Bool)
                        | (Datatype(Int), Datatype(Float))      ->   (build_sitofp lhs float_t "tmp" builder, rhs), Datatype(Float)
                        | (Datatype(Float), Datatype(Int))      ->   (lhs, build_sitofp rhs float_t "tmp" builder), Datatype(Float)
                        | (Datatype(Int), Datatype(Bool))       -> (lhs, rhs), Datatype(Bool)
                        | (Datatype(Bool), Datatype(Int))       -> (lhs, rhs), Datatype(Int)
                        | (Datatype(Int), Datatype(Vector(Int, n1))) ->
                            (lhs, rhs), Datatype(Vector(Int, n1))
                        | (Datatype(Float), Datatype(Vector(Float, n1))) ->
                            (lhs, rhs), Datatype(Vector(Float, n1))
                        | (Datatype(Vector(Int, n1)), Datatype(Vector(Int, n2))) ->
                            (lhs, rhs), Datatype(Vector(Int, n1))
                        | (Datatype(Vector(Float, n1)), Datatype(Vector(Float, n2))) ->
                            (lhs, rhs), Datatype(Vector(Float, n1))
                        | (Datatype(Int), Datatype(Matrix(Int,r1,c2))) ->
                            (lhs, rhs), Datatype(Matrix(Int, r1, c2))
                        | (Datatype(Float), Datatype(Matrix(Float,r1,c2))) ->
                            (lhs, rhs), Datatype(Matrix(Float, r1, c2))
                        | (Datatype(Matrix(Int,r1,c1)), Datatype(Matrix(Int,r2,c2))) ->
                            (lhs, rhs), Datatype(Matrix(Int, r1, c2))
                        | (Datatype(Matrix(Float,r1,c1)), Datatype(Matrix(Float,r2,c2))) ->
                            (lhs, rhs), Datatype(Matrix(Float, r1, c2))
                        | _                                 -> raise(Exceptions.IllegalCast)
                in

                let (e1ll, e2ll), d = cast e1' e2' type1 type2 in
                    let check_binop_type d =
                        match d with
                            Datatype(Int)               -> int_bops op e1ll e2ll
                            | Datatype(Float)           -> float_bops op e1ll e2ll
                            | Datatype(Bool)            -> bool_bops op e1ll e2ll
                            | Datatype(Vector(Int,n))   ->
                                let n_i = (match n with Int_lit(n1) -> n1 | _ -> -1) in
                                    vector_bops "int" n_i n op e1 e2
                            | Datatype(Vector(Float,n)) ->
                                let n_i = (match n with Int_lit(n1) -> n1 | _ -> -1) in
                                    vector_bops "float" n_i n op e1 e2
                            | Datatype(Matrix(Int,r,c)) ->
                                let r_i = (match r with Int_lit(n) -> n | _ -> -1)
                                and c_i = (match c with Int_lit(n) -> n | _ -> -1) in
                                    matrix_bops "int" r_i c_i r c op e1 e2
                            | Datatype(Matrix(Float,r,c)) ->
                                let r_i = (match r with Int_lit(n) -> n | _ -> -1)
                                and c_i = (match c with Int_lit(n) -> n | _ -> -1) in
                                    matrix_bops "float" r_i c_i r c op e1 e2
                            | _                         -> raise(Exceptions.UnsupportedBinopType)
                in
                check_binop_type d
            | S.SUnop(op, e, d)         ->
                let e' = expr builder e in
                let int_unops op =
                    (match op with
                        A.Neg     -> L.build_neg e' "tmp" builder
                        | A.Inc   -> L.build_store (L.build_add e' (L.const_int i32_t 1) "tmp" builder) (lookup (match e with S.SId(s, d) -> s | _->raise(Exceptions.IncMustBeCalledOnID))) builder
                        | A.Dec   -> L.build_store (L.build_sub e' (L.const_int i32_t 1) "tmp" builder) (lookup (match e with S.SId(s, d) -> s | _->raise(Exceptions.DecMustBeCalledOnID))) builder
                        | _       -> raise(Exceptions.IllegalIntUnop))
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
            | S.STranspose(s,d)         ->
                (match d with
                    Datatype(Matrix(Int, c, r)) ->
                        let r_tr = (match c with Int_lit(n) -> n | _ -> -1) in
                        let c_tr = (match r with Int_lit(n) -> n | _ -> -1) in
                        let tmp_tr = L.build_alloca (array_t (array_t i32_t c_tr) r_tr) "tmpmat" builder in
                        for i=0 to (r_tr-1) do
                            for j=0 to (c_tr-1) do
                                let mtr = build_matrix_access r_tr c_tr s (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                                let ld = L.build_gep tmp_tr [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                                ignore(build_store mtr ld builder);
                            done
                        done;
                        L.build_load (L.build_gep tmp_tr [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
                    | _ -> const_int i32_t 0)
            | S.SNew(p)                 -> (match p with
                                              A.Vector(_, _)  -> raise(Exceptions.CannotUseNewWithVectors)
                                            | A.Matrix(_,_,_) -> raise(Exceptions.CannotUseNewwithMatrices)
                                            | _               -> let p' = ltype_of_typ p in
                                                                 L.build_load (L.build_malloc p' "tmp" builder) "tmp2" builder)
            | S.SFree(e)                -> (match e with
                                              SId(s, d) -> L.build_free (lookup s) builder
                                            | _ -> raise(Exceptions.CanOnlyUseFreeWithVariables));
            | S.SCall ("print_string", [e], d) ->
                L.build_call printf_func [| string_format_str ; (expr builder e) |] "printf" builder
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
                let access_i = (match se1 with S.SNum_lit(SInt_lit(n)) -> n | _ -> -1)
                and access_j = (match se2 with S.SNum_lit(SInt_lit(n)) -> n | _ -> -1) in
                    (build_matrix_access access_i access_j s (L.const_int i32_t 0) i j builder false)
            | S.SVector_access (s, se, d) ->
                let i = expr builder se in
                let access_i = (match se with S.SNum_lit(SInt_lit(n)) -> n | _ -> -1) in
                    (build_vector_access access_i s (L.const_int i32_t 0) i builder false)
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
            | S.SVector_lit (sl, d) ->
                (match d with
                    A.Datatype(A.Float) ->
                        let realOrder = List.rev sl in
                        let i64List = List.map (expr builder) realOrder in
                        let arrayOfi64 = Array.of_list i64List in
                            L.const_array (array_t float_t (List.length sl)) arrayOfi64
                    | A.Datatype(A.Int) ->
                        let realOrder = List.rev sl in
                        let i32List = List.map (expr builder) realOrder in
                        let arrayOfi32 = Array.of_list i32List in
                            L.const_array (array_t i32_t (List.length sl)) arrayOfi32
                    | _ -> raise(Exceptions.UnsupportedVectorType))
            | S.SMatrix_row (s, r, d) ->
                let access_i = (match r with SNum_lit(SInt_lit(n)) -> n | _-> -1) in
                let rows = L.array_length (L.type_of (L.build_load (L.build_gep (lookup s) [| L.const_int i32_t 0 |] s builder) s builder)) in
                if (rows < access_i) then raise(Exceptions.MatrixOutOfBoundsAccess(""));
                L.build_load (L.build_gep (lookup s) [| L.const_int i32_t 0; L.const_int i32_t access_i |] s builder) s builder
            | S.SMatrix_col (s, c, d) ->
                let c_i = (match c with SNum_lit(SInt_lit(n)) -> n | _ -> -1) in
                (match d with
                    Datatype(Matrix(Int,_,_)) ->
                        let rows = L.array_length (L.type_of (L.build_load (L.build_gep (lookup s) [| L.const_int i32_t 0 |] s builder) s builder)) in
                        let cols = L.array_length (L.type_of (L.build_load (L.build_gep (lookup s) [| L.const_int i32_t 0; L.const_int i32_t 0 |] s builder) s builder)) in
                        let tmp_m = L.build_alloca (array_t i32_t cols) "tmpmat" builder in
                        for j=0 to (rows-1) do
                            let m1 = build_matrix_access j c_i s (L.const_int i32_t 0) (L.const_int i32_t j) (L.const_int i32_t c_i) builder false in
                            let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t j|] "tmpmat" builder in
                            ignore(build_store m1 ld builder);
                        done;
                        L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
                    | Datatype(Matrix(Float,_,_)) ->
                        let rows = L.array_length (L.type_of (L.build_load (L.build_gep (lookup s) [| L.const_int i32_t 0 |] s builder) s builder)) in
                        let cols = L.array_length (L.type_of (L.build_load (L.build_gep (lookup s) [| L.const_int i32_t 0; L.const_int i32_t 0 |] s builder) s builder)) in
                        let tmp_m = L.build_alloca (array_t float_t cols) "tmpmat" builder in
                        for j=0 to (rows-1) do
                            let m1 = build_matrix_access j c_i s (L.const_int i32_t 0) (L.const_int i32_t j) (L.const_int i32_t c_i) builder false in
                            let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t j|] "tmpmat" builder in
                            ignore(build_store m1 ld builder);
                        done;
                        L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
                    | _ -> L.const_int i32_t 0)
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
