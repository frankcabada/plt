open Ast
open Sast
open Exceptions
open Utils

module StringMap = Map.Make(String)

(* Keep this here please
module SS = Set.Make(
	struct
		let compare = Pervasives.compare
		type t = datatype
	end )
*)

let get_equality_binop_type type1 type2 se1 se2 op =
	if (type1 = Datatype(Float) || type2 = Datatype(Float)) then raise(Exceptions.InvalidBinopExpression "Cannot use equality for floats")
	else
	match type1, type2 with
		Datatype(String), Datatype(Int)
	|   Datatype(Int), Datatype(String) -> SBinop(se1, op, se2, Datatype(Bool))
	|   _ ->
		if type1 = type2 then SBinop(se1, op, se2, Datatype(Bool))
		else raise (Exceptions.InvalidBinopExpression "Can only use equality with Int and Strings unless same types")

let get_logical_binop_type se1 se2 op = function
	  (Datatype(Bool), Datatype(Bool)) -> SBinop(se1, op, se2, Datatype(Bool))
	| _ -> raise (Exceptions.InvalidBinopExpression "Can only use Bools for logical operators")

(* Keep this here for now please
let get_comparison_binop_type type1 type2 se1 se2 op =
	let numberics = SS.of_list [Datatype(Int); Datatype(String); Datatype(Float)]
	in
		if SS.mem type1 numberics && SS.mem type2 numberics
			then SBinop(se1, op, se2, Datatype(Bool))
		else raise (Exceptions.InvalidBinopExpression "Can only use comparison types on Int")
*)
let get_arithmetic_binop_type se1 se2 op = function
		  (Datatype(Int), Datatype(Float))
		| (Datatype(Float), Datatype(Int))
		| (Datatype(Float), Datatype(Float)) 	-> SBinop(se1, op, se2, Datatype(Float))
		| (Datatype(Int), Datatype(String))
		| (Datatype(String), Datatype(Int))
		| (Datatype(String), Datatype(String)) 	-> SBinop(se1, op, se2, Datatype(String))
		| (Datatype(Int), Datatype(Int)) 		-> SBinop(se1, op, se2, Datatype(Int))
		| _ -> raise (Exceptions.InvalidBinopExpression "Arithmetic operators onlu supprts Int, Float, and String")

let rec get_ID_type s func_st =
	try StringMap.find s func_st
	with | Not_found -> raise (Exceptions.UndefinedID(s))

and check_assign func_st s e =
	let type1 = get_ID_type s func_st in
	let se = expr_to_sexpr func_st e in
	let type2 = get_type_from_sexpr se in
	match type1, type2 with
		Datatype(String), Datatype(Int)
		| Datatype(Int), Datatype(String) -> SAssign(s, se, type1)
		| _ ->
	if type1 = type2
		then SAssign(s, se, type1)
	else raise(Exceptions.AssignmentTypeMismatch(Utils.string_of_datatype type1, Utils.string_of_datatype type2))

and check_unop func_st op e =
	let check_num_unop t = function
		  Neg 		-> t
		| Not 		-> t
		| Inc     -> t
		| Dec     -> t
	in
	let check_bool_unop x = match x with
			Not 	-> Datatype(Bool)
		| 	_ 		-> raise(Exceptions.InvalidUnaryOperation)
	in
	let se = expr_to_sexpr func_st e in
	let t = get_type_from_sexpr se in
		match t with
		  Datatype(Int)
		| Datatype(Float) -> SUnop(op, se, check_num_unop t op)
		| Datatype(Bool)  -> SUnop(op, se, check_bool_unop op)
		| _ 			  -> raise(Exceptions.InvalidUnaryOperation)

and check_binop func_st e1 op e2 =
	let se1 = expr_to_sexpr func_st e1 in
	let se2 = expr_to_sexpr func_st e2 in
	let type1 = get_type_from_sexpr se1 in
	let type2 = get_type_from_sexpr se2 in
	match op with
	Equal | Neq -> get_equality_binop_type type1 type2 se1 se2 op
	| And | Or -> get_logical_binop_type se1 se2 op (type1, type2)
	| Less | Leq | Greater | Geq when type1 = Datatype(Int) && type2 = Datatype(Int) -> SBinop(se1, op, se2, Datatype(Bool))
	| Add | Mult | Sub | Div -> get_arithmetic_binop_type se1 se2 op (type1, type2)
	| _ -> raise (Exceptions.InvalidBinopExpression ((Utils.string_of_op op) ^ " is not a supported binary op"))
	(*	| Less | Leq | Greater | Geq -> get_comparison_binop_type type1 type2 se1 se2 op *)

and check_matrix_init func_st e1 e2 e3 =
	ignore(check_expr_is_int_lit e1);
	ignore(check_expr_is_int_lit e2);
	ignore(check_expr_is_int_lit e3);
	SMatrix_init(expr_to_sexpr func_st e1, expr_to_sexpr func_st e2, expr_to_sexpr func_st e3)

and check_expr_is_int_lit e = match e with
	Num_lit(Int_lit(_)) -> ()
	| _ -> raise(Exceptions.InvalidMatrixInit)

and check_matrix_row func_st s e =
	ignore(check_expr_is_int_lit e);
	let t = get_ID_type s func_st	in
		match t with
			Datatype(Matrix(_,_,_)) -> SMatrix_row(s, expr_to_sexpr func_st e)
			| _ -> raise(Exceptions.MatrixRowOnNonMatrix(s))

and check_matrix_col func_st s e =
	ignore(check_expr_is_int_lit e);
	let t = get_ID_type s func_st	in
		match t with
			Datatype(Matrix(_,_,_)) -> SMatrix_col(s, expr_to_sexpr func_st e)
			| _ -> raise(Exceptions.MatrixColOnNonMatrix(s))

and check_matrix_access func_st s e1 e2 =
	ignore(check_expr_is_int_lit e1);
	ignore(check_expr_is_int_lit e2);
	let t = get_ID_type s func_st	in
		match t with
			Datatype(Matrix(_,_,_)) -> SMatrix_access(s, expr_to_sexpr func_st e1, expr_to_sexpr func_st e2)
			| _ -> raise(Exceptions.MatrixAccOnNonMatrix(s))

and function_decl s fname_map =
	try StringMap.find s fname_map
	with Not_found -> raise (Exceptions.FunctionNotFound(s))

and expr_to_sexpr func_st = function
	  Num_lit(Int_lit(n))     -> SNum_lit(SInt_lit(n))
	| Num_lit(Float_lit(n))   -> SNum_lit(SFloat_lit(n))
	| Bool_lit(b)       	  	-> SBool_lit(b)
	| String_lit(s)           -> SString_lit(s)
	| Id(s)                   -> SId(s, get_ID_type s func_st)
	| Null                    -> SNull
	| Noexpr                  -> SNoexpr
	| Unop(op, e)             -> check_unop func_st op e
	| Assign(s, e)   		  		-> check_assign func_st s e
	| Binop(e1, op, e2)       -> check_binop func_st e1 op e2
	| Matrix_init(e1, e2, e3) -> check_matrix_init func_st e1 e2 e3
	| Matrix_row(s, e)       	-> check_matrix_row func_st s e
	| Matrix_col(s, e)       	-> check_matrix_col func_st s e
	| Matrix_access(s, e1, e2)-> check_matrix_access func_st s e1 e2
(*
	| Call(s, el) as call -> let fd = function_decl s in
		if List.length el != List.length fd.formals then
			raise (Exceptions.IncorrectNumberOfArguments(fd.fname, List.length el, List.length fd.formals))
	| Matrix_lit(el)          -> check_matrix_lit el
*)

and get_type_from_sexpr sexpr = match sexpr with
	  SNum_lit(SInt_lit(_))		-> Datatype(Int)
	| SNum_lit(SFloat_lit(_))	-> Datatype(Float)
	| SBool_lit(_)						-> Datatype(Bool)
	| SString_lit(_) 					-> Datatype(String)
	| SNoexpr 								-> Datatype(Void)
	| SNull										-> Datatype(Void)
	| SId(_, d) 							-> d
	| SBinop(_, _, _, d) 			-> d
	| SAssign(_, _, d) 				-> d
	| SCall(_, _, d)					-> d
	| SUnop(_, _, d) 					-> d

let add_reserved_functions =
	let reserved_stub name return_type formals =
		{
			sreturn_type			= return_type;
			sfname 					= name;
			sformals 				= formals;
			slocals					= [];
			sbody 					= [];
		}
	in
	let void_t = Datatype(Void) in
	let str_t = Datatype(String) in
	let mf t n = Formal(t, n) in (* Make formal *)
	let reserved = [
		reserved_stub "print_line" 	(void_t) 	([mf str_t "string_in"]);
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
		| _ 									-> ()

let check_not_void_formal form =
	match form with
		Formal(Datatype(Void), n) 	-> raise(Exceptions.VoidFunctionFormal(n))
		| _ 												-> ()

let check_not_void_local local =
	match local with
		Local(Datatype(Void), n) 	-> raise(Exceptions.VoidFunctionLocal(n))
		| _ 											-> ()

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

let fdecl_to_func_st fdecl =
	let funcst = List.fold_left (fun m f -> StringMap.add (get_formal_id f) (get_formal_type f) m) StringMap.empty fdecl.formals in
		List.fold_left (fun m l -> StringMap.add (get_local_id l) (get_local_type l) m) funcst fdecl.locals

let rec stmt_to_sstmt func_st = function
	Return(e)				-> SReturn(expr_to_sexpr func_st e)
	| Block(sl) 			-> SBlock(convert_stmt_list_to_sstmt_list func_st sl)
	| Expr(e) 				-> SExpr(expr_to_sexpr func_st e)
	| If(e, s1, s2) 		-> SIf((expr_to_sexpr func_st e), (stmt_to_sstmt func_st s1), (stmt_to_sstmt func_st s2))
	| Else(s) 				-> SElse(stmt_to_sstmt func_st s)
	| For(e1, e2, e3, s) 	-> SFor((expr_to_sexpr func_st e1), (expr_to_sexpr func_st e2), (expr_to_sexpr func_st e3), (stmt_to_sstmt func_st s))
	| While(e, s)			-> SWhile((expr_to_sexpr func_st e), (stmt_to_sstmt func_st s))

and convert_stmt_list_to_sstmt_list func_st stmt_list = List.map (stmt_to_sstmt func_st) stmt_list

let function_to_fname_map fdecls =
	List.fold_left
		(fun m fd -> StringMap.add fd.fname fd m) StringMap.empty fdecls

let convert_fdecl_to_sfdecl reserved fdecl =
	{
		sfname 				= fdecl.fname;
		sreturn_type 	= fdecl.return_type;
		sformals 			= fdecl.formals;
		slocals 			= fdecl.locals;
		sbody 				= (convert_stmt_list_to_sstmt_list (fdecl_to_func_st fdecl) fdecl.body);
	}

let check_function_return fname fbody returnType =
	let len = List.length fbody in
		if len = 0 then () else
		let final_stmt = List.hd (List.rev fbody) in
			match returnType, final_stmt with
				Datatype(Void), Return(_) -> raise(Exceptions.AllVoidFunctionsMustNotReturn(fname))
				|	Datatype(Void), _ 	  -> ()
				| _, Return(_)	 	      -> ()
				| _, _					  -> raise(Exceptions.AllNonVoidFunctionsMustEndWithReturn(fname))

let check_return fdecl func_st e =
	let se = expr_to_sexpr func_st e in
	let t = get_type_from_sexpr se in
		if (t=fdecl.return_type) then () else raise(Exceptions.ReturnTypeMismatch(Utils.string_of_datatype t, Utils.string_of_datatype fdecl.return_type))

let rec check_stmt fdecl = function
	Return(e) 				-> check_return fdecl (fdecl_to_func_st fdecl) e
	| Block(sl) 			-> check_fbody fdecl sl
	| If(e, s1, s2) 		-> check_if fdecl s1 s2
	| While(e, s)			-> check_while fdecl s
	| For(e1, e2, e3, s) 	-> check_for fdecl s
	| Else(s)				-> check_else fdecl s
	| Expr(e)				-> ()

and check_fbody fdecl fbody =
	ignore(List.iter (check_stmt fdecl) fbody);

and check_if fdecl s1 s2 =
	ignore(check_stmt fdecl s1);
	ignore(check_stmt fdecl s2);

and check_while fdecl stmt =
	ignore(check_stmt fdecl stmt);

and check_for fdecl stmt =
	ignore(check_stmt fdecl stmt);

and check_else fdecl stmt =
	ignore(check_stmt fdecl stmt);;

let check_function global_st fdecl =
	ignore(List.iter check_not_void_formal fdecl.formals);
	ignore(List.iter check_not_void_local fdecl.locals);
	ignore(report_duplicate "function" ((List.map get_formal_id fdecl.formals) @ (List.map get_local_id fdecl.locals)));
	ignore(check_function_return fdecl.fname fdecl.body fdecl.return_type);
	ignore(check_fbody fdecl fdecl.body);;

let check_functions global_st globals fdecls =
	let sast =
		ignore(report_duplicate "function" (List.map (fun fd -> fd.fname) fdecls));
		ignore(List.iter (check_function global_st) fdecls);
		let sfdecls = List.map (convert_fdecl_to_sfdecl add_reserved_functions) fdecls in
		(globals, sfdecls)
		in sast
