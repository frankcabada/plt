open Ast
open Sast
open Exceptions
open Utils

module StringMap = Map.Make(String)

let get_equality_binop_type type1 type2 se1 se2 op =
	if (type1 = Datatype(Float) || type2 = Datatype(Float)) then raise(Exceptions.InvalidBinopExpression "Cannot use equality for floats")
	else
		if type1 = type2 && (type1 = Datatype(String) || type1 = Datatype(Int)) then SBinop(se1, op, se2, type1)
		else raise (Exceptions.InvalidBinopExpression("Can only use equality operators with ints and Strings"))

let get_logical_binop_type se1 se2 op = function
		| (Datatype(Bool), Datatype(Bool)) -> SBinop(se1, op, se2, Datatype(Bool))
		| _ -> raise (Exceptions.InvalidBinopExpression "Can only use Bools for logical operators")

let get_arithmetic_binop_type se1 se2 op = function
		  (Datatype(Int), Datatype(Float))
		| (Datatype(Float), Datatype(Int))
		| (Datatype(Float), Datatype(Float)) 	-> SBinop(se1, op, se2, Datatype(Float))
		| (Datatype(String), Datatype(String)) 	->
			(match op with
				Add -> SBinop(se1, op, se2, Datatype(String))
				| _ -> raise(Exceptions.UnsupportedStringBinop("Cannot subtract, multiply, or divide strings")))
		| (Datatype(Int), Datatype(Int)) 		-> SBinop(se1, op, se2, Datatype(Int))
		| (Datatype(Vector(typ1, n1)), Datatype(Vector(typ2, n2))) ->
			(match op with
				Add | Sub 	->
					if typ1=typ2 && n1=n2 then
						SBinop(se1, op, se2, Datatype(Vector(typ1, n1)))
					else raise(Exceptions.MismatchedVectorsForBinop("Vectors must be same type and size for +/-"))
				| _ 		-> raise(Exceptions.UnsupportedVectorBinop("Cannot multiply or divide vectors")))
		| (Datatype(Matrix(typ1, i1, j1)), Datatype(Matrix(typ2, i2, j2))) ->
			(match op with
				Add | Sub 	->
					if typ1=typ2 && i1=i2 && j1=j2 then
						SBinop(se1, op, se2, Datatype(Matrix(typ1, i1, j2)))
					else raise(Exceptions.MismatchedMatricesForAddSub("Matrices must be same type and dimensions for +/-"))
				| Mult 		->
					if typ1=typ2 && j1 = i2 then
						SBinop(se1, op, se2, Datatype(Matrix(typ1, i1, j2)))
					else raise(Exceptions.MismatchedMatricesForMult("Matrices M1(i1,j1) and M2(i2,j2) must have j1 = i2 and be of same type to be multiplied"))
				| _ -> raise(Exceptions.UnsupportedMatrixBinop("Cannot divide matrices")))
		| (Datatype(Int), Datatype(Vector(Int, n))) -> SBinop(se1, op, se2, Datatype(Vector(Int, n)))
		| (Datatype(Int), Datatype(Matrix(Int,i,j))) -> SBinop(se1, op, se2, Datatype(Matrix(Int, i, j)))
		| (Datatype(Float), Datatype(Vector(Float, n))) -> SBinop(se1, op, se2, Datatype(Vector(Float, n)))
		| (Datatype(Float), Datatype(Matrix(Float,i,j))) -> SBinop(se1, op, se2, Datatype(Matrix(Float, i, j)))
		| _ -> raise (Exceptions.InvalidBinopExpression("Arithmetic operators on unsupported type"))

let rec get_ID_type s func_st =
	try StringMap.find s func_st
	with | Not_found -> raise (Exceptions.UndefinedID(s))

and check_assign fname_map func_st e1 e2 =
	let se1 = expr_to_sexpr fname_map func_st e1 in
	let type1 = get_type_from_sexpr se1 in
	let se2 = expr_to_sexpr fname_map func_st e2 in
	let type2 = get_type_from_sexpr se2 in
	match type1, type2 with
		Datatype(String), Datatype(Int)
		| Datatype(Int), Datatype(String) -> SAssign(se1, se2, type1)
		| _ ->
	if type1 = type2
		then SAssign(se1, se2, type1)
	else raise(Exceptions.AssignmentTypeMismatch(Utils.string_of_datatype type1, Utils.string_of_datatype type2))

and check_unop fname_map func_st op e =
	let check_num_unop t = function
		  Neg -> t
		| Not -> t
		| Inc -> t
		| Dec -> t
	in
	let check_bool_unop x = match x with
			Not 	-> Datatype(Bool)
		| 	_ 		-> raise(Exceptions.InvalidUnaryOperation)
	in
	let se = expr_to_sexpr fname_map func_st e in
	let t = get_type_from_sexpr se in
		match t with
		  Datatype(Int)
		| Datatype(Float) -> SUnop(op, se, check_num_unop t op)
		| Datatype(Bool)  -> SUnop(op, se, check_bool_unop op)
		| _ 			  -> raise(Exceptions.InvalidUnaryOperation)

and check_binop fname_map func_st e1 op e2 =
	let se1 = expr_to_sexpr fname_map func_st e1 in
	let se2 = expr_to_sexpr fname_map func_st e2 in
	let type1 = get_type_from_sexpr se1 in
	let type2 = get_type_from_sexpr se2 in
	match op with
	Equal | Neq -> get_equality_binop_type type1 type2 se1 se2 op
	| And | Or -> get_logical_binop_type se1 se2 op (type1, type2)
	| Less | Leq | Greater | Geq when type1 = type2 && (type1 = Datatype(Int) || type1 = Datatype(Float)) -> SBinop(se1, op, se2, type1)
	| Add | Mult | Sub | Div -> get_arithmetic_binop_type se1 se2 op (type1, type2)
	| _ -> raise (Exceptions.InvalidBinopExpression ((Utils.string_of_op op) ^ " is not a supported binary op"))

and check_matrix_init fname_map func_st e1 e2 e3 =
	ignore(check_expr_is_int_lit e1);
	ignore(check_expr_is_int_lit e2);
	ignore(check_expr_is_int_lit e3);
	SMatrix_init(expr_to_sexpr fname_map func_st e1, expr_to_sexpr fname_map func_st e2, expr_to_sexpr fname_map func_st e3, Datatype(Int))

and check_expr_is_int_lit e = match e with
	Num_lit(Int_lit(n)) -> n
	| _ -> raise(Exceptions.MatrixDimensionMustBeIntLit)

and check_matrix_row fname_map func_st s e =
	ignore(check_expr_is_int_lit e);
	let t = get_ID_type s func_st	in
		match t with
			Datatype(Matrix(d,_,_)) -> SMatrix_row(s, expr_to_sexpr fname_map func_st e, Datatype(d))
			| _ -> raise(Exceptions.MatrixRowOnNonMatrix(s))

and check_matrix_col fname_map func_st s e =
	ignore(check_expr_is_int_lit e);
	let t = get_ID_type s func_st	in
		match t with
			Datatype(Matrix(d,_,_)) -> SMatrix_col(s, expr_to_sexpr fname_map func_st e, Datatype(d))
			| _ -> raise(Exceptions.MatrixColOnNonMatrix(s))

and check_matrix_access fname_map func_st s e1 e2 =
	let i = check_expr_is_int_lit e1 in
	let j = check_expr_is_int_lit e2 in
	let t = get_ID_type s func_st	in
		match t with
			Datatype(Matrix(d,rows,cols)) ->
				let r = (match rows with Int_lit(n) -> n | _ -> raise(Exceptions.MatrixDimensionMustBeIntLit))
				and c = (match cols with Int_lit(n) -> n | _ -> raise(Exceptions.MatrixDimensionMustBeIntLit)) in
				if i < r && j < c
					then SMatrix_access(s, expr_to_sexpr fname_map func_st e1, expr_to_sexpr fname_map func_st e2, Datatype(d))
					else raise(Exceptions.MatrixOutOfBoundsAccess(s))
			| _ -> raise(Exceptions.MatrixAccessOnNonMatrix(s))

and check_vector_access fname_map func_st s e =
	ignore(check_expr_is_int_lit e);
	let t = get_ID_type s func_st	in
		match t with
			Datatype(Vector(d,_)) -> SVector_access(s, expr_to_sexpr fname_map func_st e, Datatype(d))
			| _ -> raise(Exceptions.VectorAccessOnNonMatrix(s))

and check_matrix_lit fname_map func_st el =
	let sl = List.map (expr_to_sexpr fname_map func_st) el in
	  	let t = get_type_from_sexpr (List.hd sl) in
			ignore(List.iter (fun sexpr ->
						if t = get_type_from_sexpr sexpr then ()
						else raise(Exceptions.MatrixLitMustBeOneType)) sl);
			SMatrix_lit(sl, t)

and function_decl s fname_map =
	try StringMap.find s fname_map
	with Not_found -> raise (Exceptions.FunctionNotFound(s))

and expr_to_sexpr fname_map func_st = function
	  Num_lit(Int_lit(n))  	-> SNum_lit(SInt_lit(n))
	| Num_lit(Float_lit(n))	-> SNum_lit(SFloat_lit(n))
	| Bool_lit(b)       	-> SBool_lit(b)
	| String_lit(s)        	-> SString_lit(s)
	| Id(s)                	-> SId(s, get_ID_type s func_st)
	| Null                 	-> SNull
	| Noexpr               	-> SNoexpr
	| Unop(op, e)          	-> check_unop fname_map func_st op e
	| Assign(s, e)   		-> check_assign fname_map func_st s e
	| Binop(e1, op, e2)    	-> check_binop fname_map func_st e1 op e2
	| Call(s, el)			-> let fd = function_decl s fname_map in
		if List.length el != List.length fd.formals then
			raise (Exceptions.IncorrectNumberOfArguments(fd.fname, List.length el, List.length fd.formals))
		else
		SCall(s, List.map (expr_to_sexpr fname_map func_st) el, fd.return_type)
	| Vector_access(s, e)				-> check_vector_access fname_map func_st s e
	| Matrix_access(s, e1, e2)	-> check_matrix_access fname_map func_st s e1 e2
	| Matrix_init(e1, e2, e3) 	-> check_matrix_init fname_map func_st e1 e2 e3
	| Matrix_row(s, e)       		-> check_matrix_row fname_map func_st s e
	| Matrix_col(s, e)       		-> check_matrix_col fname_map func_st s e
	| Matrix_lit(el)						-> check_matrix_lit fname_map func_st el

and get_type_from_sexpr sexpr = match sexpr with
	  SNum_lit(SInt_lit(_))				-> Datatype(Int)
	| SNum_lit(SFloat_lit(_))			-> Datatype(Float)
	| SBool_lit(_)						-> Datatype(Bool)
	| SString_lit(_) 					-> Datatype(String)
	| SNoexpr 							-> Datatype(Void)
	| SNull								-> Datatype(Void)
	| SId(_, d) 						-> d
	| SBinop(_, _, _, d) 				-> d
	| SAssign(_, _, d) 					-> d
	| SCall(_, _, d)					-> d
	| SUnop(_, _, d) 					-> d
	| SVector_access(_, _, d)			-> d
	| SMatrix_access(_, _, _, d)		-> d
	| SMatrix_init(_, _, _, d)			-> d
	| SMatrix_row(_, _, d)				-> d
	| SMatrix_col(_, _, d)				-> d
	| SMatrix_lit(_, d)					-> d

let add_reserved_functions =
	let reserved_stub name return_type formals =
		{
			return_type			= return_type;
			fname 					= name;
			formals 				= formals;
			locals					= [];
			body 						= [];
		}
	in
	let void_t = Datatype(Void) in
	let str_t = Datatype(String) in
	let i32_t = Datatype(Int) in
	let float_t = Datatype(Float) in
	let mf t n = Formal(t, n) in (* Make formal *)
	let reserved = [
		reserved_stub "print_string" 	(void_t) 	([mf str_t "string_in"]);
		reserved_stub "print_int"		(void_t)	([mf i32_t "int_in"]);
		reserved_stub "print_float"		(void_t)	([mf float_t "float_in"]);
	] in
	reserved

(* Variable Declaration Checking Functions *)

let report_duplicate s li =
	let rec helper = function
		n1 :: n2 :: _ when n1 = n2 ->
			if s = "global" then raise(Exceptions.DuplicateGlobal(n1))
			else if s = "function" then raise(Exceptions.DuplicateFunc(n1))
		| _ :: t -> helper t
		| [] -> ()
	in helper (List.sort compare li)

let check_not_void s var_decl =
	match var_decl with
		(Datatype(Void), n) 	->
			if s = "global" then raise(Exceptions.VoidGlobal(n))
			else if s = "function" then raise(Exceptions.VoidFunc(n))
		| _ 					-> ()

let check_not_void_formal form =
	match form with
		Formal(Datatype(Void), n) 	-> raise(Exceptions.VoidFunctionFormal(n))
		| _ 					 	-> ()

let check_not_void_local local =
	match local with
		Local(Datatype(Void), n) 	-> raise(Exceptions.VoidFunctionLocal(n))
		| _ 						-> ()

let add_to_global_symbol_table globs =
	List.fold_left
		(fun m (t,n) -> StringMap.add n t m) StringMap.empty globs

let get_formal_id = function
	| Formal(Datatype(p), n) -> n

let get_formal_type = function
	| Formal(Datatype(p), n) -> Datatype(p)

let get_local_id = function
	| Local(Datatype(p), n) -> n

let get_local_type = function
	| Local(Datatype(p), n) -> Datatype(p)

let check_var_decls globals =
	ignore(List.iter (check_not_void "global") globals);
	ignore(report_duplicate "global" (List.map snd globals));
	add_to_global_symbol_table globals;;

(* Function Declaration Checking Functions *)

let fdecl_to_func_st globals fdecl =
	let ffunc_st = List.fold_left (fun m f -> StringMap.add (get_formal_id f) (get_formal_type f) m) StringMap.empty fdecl.formals in
		let lffunc_st = List.fold_left (fun m l -> StringMap.add (get_local_id l) (get_local_type l) m) ffunc_st fdecl.locals in
			List.fold_left (fun m g -> StringMap.add (snd g) (fst g) m) lffunc_st globals

let rec stmt_to_sstmt fname_map func_st = function
	  Return(e)				-> SReturn(expr_to_sexpr fname_map func_st e)
	| Block(sl) 			-> SBlock(convert_stmt_list_to_sstmt_list fname_map func_st sl)
	| Expr(e) 				-> SExpr(expr_to_sexpr fname_map func_st e)
	| If(e, s1, s2) 		-> SIf((expr_to_sexpr fname_map func_st e), (stmt_to_sstmt fname_map func_st s1), (stmt_to_sstmt fname_map func_st s2))
	| For(e1, e2, e3, s) 	-> SFor((expr_to_sexpr fname_map func_st e1), (expr_to_sexpr fname_map func_st e2), (expr_to_sexpr fname_map func_st e3), (stmt_to_sstmt fname_map func_st s))
	| While(e, s)			-> SWhile((expr_to_sexpr fname_map func_st e), (stmt_to_sstmt fname_map func_st s))

and convert_stmt_list_to_sstmt_list fname_map func_st stmt_list = List.map (stmt_to_sstmt fname_map func_st) stmt_list

let fdecls_to_fname_map fdecls =
	List.fold_left
		(fun m fd -> StringMap.add fd.fname fd m) StringMap.empty (fdecls @ add_reserved_functions)

let convert_fdecl_to_sfdecl globs fname_map fdecl =
	{
		sfname 				= fdecl.fname;
		sreturn_type 		= fdecl.return_type;
		sformals 			= fdecl.formals;
		slocals 			= fdecl.locals;
		sbody 				= (convert_stmt_list_to_sstmt_list fname_map (fdecl_to_func_st globs fdecl) fdecl.body);
	}

let check_function_return fname fbody returnType =
	let len = List.length fbody in
		if len = 0 && returnType != Datatype(Void) then
			raise(Exceptions.AllNonVoidFunctionsMustEndWithReturn(fname))
		else
		let final_stmt = List.hd (List.rev fbody) in
			match returnType, final_stmt with
				Datatype(Void), Return(_) -> raise(Exceptions.AllVoidFunctionsMustNotReturn(fname))
				|	Datatype(Void), _ 	  -> ()
				| _, Return(_)	 	      -> ()
				| _, _					  -> raise(Exceptions.AllNonVoidFunctionsMustEndWithReturn(fname))

let check_return fname_map fdecl func_st e =
	let se = expr_to_sexpr fname_map func_st e in
	let t = get_type_from_sexpr se in
		if (t=fdecl.return_type) then () else raise(Exceptions.ReturnTypeMismatch(Utils.string_of_datatype t, Utils.string_of_datatype fdecl.return_type))

let rec check_stmt globs fname_map fdecl = function
	Return(e) 					-> check_return fname_map fdecl (fdecl_to_func_st globs fdecl) e
	| Block(sl) 				-> check_fbody globs fname_map fdecl sl
	| If(e, s1, s2) 			-> check_if globs fname_map fdecl s1 s2
	| While(e, s)				-> check_while globs fname_map fdecl s
	| For(e1, e2, e3, s)		-> check_for globs fname_map fdecl s
	| Expr(e)					-> ()

and check_fbody globs fname_map fdecl fbody =
	ignore(List.iter (check_stmt globs fname_map fdecl) fbody);

and check_if globs fname_map fdecl s1 s2 =
	ignore(check_stmt globs fname_map fdecl s1);
	ignore(check_stmt globs fname_map fdecl s2);

and check_while globs fname_map fdecl stmt =
	ignore(check_stmt globs fname_map fdecl stmt);

and check_for globs fname_map fdecl stmt =
	ignore(check_stmt globs fname_map fdecl stmt);

and check_else globs fname_map fdecl stmt =
	ignore(check_stmt globs fname_map fdecl stmt);;

let check_function globals fname_map global_st fdecl =
	ignore(List.iter check_not_void_formal fdecl.formals);
	ignore(List.iter check_not_void_local fdecl.locals);
	ignore(report_duplicate "function" ((List.map get_formal_id fdecl.formals) @ (List.map get_local_id fdecl.locals)));
	ignore(check_function_return fdecl.fname fdecl.body fdecl.return_type);
	ignore(check_fbody globals fname_map fdecl fdecl.body);;

let check_functions global_st globals fdecls =
	let sast =
		let fname_map = fdecls_to_fname_map fdecls in
		ignore(report_duplicate "function" (List.map (fun fd -> fd.fname) fdecls));
		ignore(List.iter (check_function globals fname_map global_st) fdecls);
		let sfdecls = List.map (convert_fdecl_to_sfdecl globals fname_map) fdecls in
		(globals, sfdecls)
	in sast
