open Ast
open Sast
open Exceptions
open Utils

module StringMap = Map.Make(String)

type env = {
	env_name      	: string;
	env_locals    	: datatype StringMap.t;
	env_parameters	: Ast.formal StringMap.t;
	env_returnType	: datatype;
	env_in_for    	: bool;
	env_in_while  	: bool;
	env_reserved    : sfunc_decl list;
}

let update_env_name env env_name =
{
	env_name       = env_name;
	env_locals     = env.env_locals;
	env_parameters = env.env_parameters;
	env_returnType = env.env_returnType;
	env_in_for     = env.env_in_for;
	env_in_while   = env.env_in_while;
	env_reserved   = env.env_reserved;
}

let update_call_stack env in_for in_while =
{
	env_name       = env.env_name;
	env_locals     = env.env_locals;
	env_parameters = env.env_parameters;
	env_returnType = env.env_returnType;
	env_in_for     = in_for;
	env_in_while   = in_while;
	env_reserved   = env.env_reserved;
}

let rec get_ID_type env s =
	try StringMap.find s env.env_locals
	with | Not_found ->
	try let formal = StringMap.find s env.env_parameters in
		(function Formal(t, _) -> t ) formal
	with | Not_found -> raise (Exceptions.UndefinedID s)

and check_unop env op e =
	let check_num_unop t = function
			Neg 		-> t
		| _ 			-> raise(Exceptions.InvalidUnaryOperation)
	in
	let check_bool_unop = function
	Not 	-> Datatype(Bool)
	| 	_ 		-> raise(Exceptions.InvalidUnaryOperation)
	in
	let se, env = expr_to_sexpr env e in
	let t = get_type_from_sexpr se in
		match t with
			Datatype(Int)
		|	Datatype(Float) 	-> SUnop(op, se, check_num_unop t op)
		| Datatype(Bool) 		-> SUnop(op, se, check_bool_unop op)
		| _ 								-> raise(Exceptions.InvalidUnaryOperation)

and expr_to_sexpr env = function
	(*	Num_lit i           -> SNum_lit(i), env*)
	|   Bool_lit b       		-> SBool_lit(b), env
	|   String_lit s        -> SString_lit(s), env
	|   Id s                -> SId(s, get_ID_type env s), env
	|   Null                -> SNull, env
	|   Noexpr              -> SNoexpr, env
(*	|   Call(s, el)         -> check_call_type env false env s el, env *)
(*	|   Assign(s, e2)      	-> check_assign env s e2, env *)
	|   Unop(op, e)         -> check_unop env op e, env
(*	|   Binop(e1, op, e2)   -> check_binop env e1 op e2, env *)

and get_type_from_sexpr = function
		SNum_lit(_)						-> Datatype(Int)
	| 	SBool_lit(_)				-> Datatype(Bool)
	| 	SString_lit(_) 			-> Datatype(String)
	| 	SId(_, d) 					-> d
	| 	SBinop(_, _, _, d) 	-> d
	| 	SAssign(_, _, d) 		-> d
	| 	SNoexpr 						-> Datatype(Void)
(*	| 	SCall(_, _, d, _)			-> d*)
	|  	SUnop(_, _, d) 			-> d
	| 	SNull								-> Datatype(Void)

let get_arithmetic_binop_type se1 se2 op = function
			(Datatype(Int), Datatype(Float))
		| 	(Datatype(Float), Datatype(Int))
		| 	(Datatype(Float), Datatype(Float)) 	-> SBinop(se1, op, se2, Datatype(Float))

		| 	(Datatype(Int), Datatype(Int)) 			-> SBinop(se1, op, se2, Datatype(Int))

		| _ -> raise (Exceptions.InvalidBinopExpression "Arithmetic operators don't support these types")

let rec check_sblock sl env = match sl with
		[] -> SBlock([SExpr(SNoexpr, Datatype(Void))]), env
	| 	_  ->
		let sl, _ = convert_stmt_list_to_sstmt_list env sl in
		SBlock(sl), env

and check_expr_stmt e env =
	let se, env = expr_to_sexpr env e in
	let t = get_type_from_sexpr se in
	SExpr(se, t), env

and check_return e env =
	let se, _ = expr_to_sexpr env e in
	let t = get_type_from_sexpr se in
	match t, env.env_returnType with
		(*Datatype(Null), Datatype(Objecttype(_))
	| 	Datatype(Null), Arraytype(_, _) -> SReturn(se, t), env*)
	 	_ ->
	if t = env.env_returnType
		then SReturn(se, t), env
		else raise (Exceptions.ReturnTypeMismatch(Utils.string_of_datatype t, Utils.string_of_datatype env.env_returnType))

and parse_stmt env = function
	(*	Block sl 				-> check_sblock sl env
	| 	Expr e 					-> check_expr_stmt e env
	|*) Return e 				-> check_return e env
	(*| 	If(e, s1, s2) 			-> check_if e s1 s2	env
	| 	For(e1, e2, e3, e4) 	-> check_for e1 e2 e3 e4 env
	| 	While(e, s)				-> check_while e s env
	|  	Break 					-> check_break env (* Need to check if in right context *)
	|   Continue 				-> check_continue env (* Need to check if in right context *)
	|   Local(d, s, e) 			-> local_handler d s e env*)

(* Update this function to return an env object *)
and convert_stmt_list_to_sstmt_list env stmt_list =
	let env_ref = ref(env) in
	let rec iter = function
	  head::tail ->
		let a_head, env = parse_stmt !env_ref head in
		env_ref := env;
		a_head::(iter tail)
	| [] -> []
	in
	let sstmt_list = (iter stmt_list), !env_ref in
	sstmt_list

let check_fbody fname fbody returnType =
	let len = List.length fbody in
	if len = 0 then () else
	let final_stmt = List.hd (List.rev fbody) in
	match returnType, final_stmt with
		Datatype(Void), _ -> ()
	| 	_, SReturn(_, _) -> ()
	| 	_ -> raise(Exceptions.AllNonVoidFunctionsMustEndWithReturn(fname))

let convert_fdecl_to_sfdecl reserved fname fdecl =
	let env_param_helper m fname = match fname with
			Formal(d, s) -> (StringMap.add s fname m)
	in
	let env_params = List.fold_left env_param_helper StringMap.empty (fdecl.formals) in
	let env = {
		env_name     	= fname;
		env_locals    	= StringMap.empty;
		env_parameters	= env_params;
		env_returnType	= fdecl.return_type;
		env_in_for 		= false;
		env_in_while 	= false;
		env_reserved 	= reserved;
	}
	in
	let get_name fdecl = fdecl.fname in
	let fbody = fst (convert_stmt_list_to_sstmt_list env fdecl.body) in
	let fname = (get_name fdecl) in ignore(check_fbody fname fbody fdecl.return_type);
	(* We add the class as the first parameter to the function for codegen *)
	{
		sfname 				= get_name fdecl;
		sreturn_type 	= fdecl.return_type;
		sformals 			= fdecl.formals;
		slocals 			= fdecl.locals;
		sbody 				= fbody;
	}


let add_reserved_functions =
	let reserved_stub name return_type formals =
		{
			sreturn_type		= return_type;
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

	let report_duplicate_func li =
	let rec helper = function
	n1 :: n2 :: _ when n1 = n2 -> raise(Exceptions.DuplicateFunc(n1))
	| _ :: t -> helper t
	| [] -> ()
	in helper (List.sort compare li)

	let check_not_void_func_vars var_decl =
	match var_decl with
	(Void, n) -> raise(Exceptions.VoidFunc(n))
	| _ -> ()

let report_duplicate_global li =
	let rec helper = function
		n1 :: n2 :: _ when n1 = n2 -> raise(Exceptions.DuplicateGlobal(n1))
		| _ :: t -> helper t
		| [] -> ()
	in helper (List.sort compare li)

let check_not_void_global var_decl =
	match var_decl with
		  (Datatype(Void), n) -> raise(Exceptions.VoidGlobal(n))
		| _ -> ()

let check_var_decls globals =
	ignore(List.iter check_not_void_global globals);
	ignore(report_duplicate_global (List.map snd globals));
	ignore();

(*and check_assign env s e =
	let se1, env = expr_to_sexpr env s in
	let se2, env = expr_to_sexpr env e in
	let type1 = get_type_from_sexpr se1 in
	let type2 = get_type_from_sexpr se2 in
	if type1 = type2
		then SAssign(se1, se2, type1)
		else raise (Exceptions.AssignmentTypeMismatch(Utils.string_of_datatype type1, Utils.string_of_datatype type2))
and check_binop env e1 op e2 =
	let se1, env = expr_to_sexpr env e1 in
	let se2, env = expr_to_sexpr env e2 in
	let type1 = get_type_from_sexpr se1 in
	let type2 = get_type_from_sexpr se2 in
	match op with
	Equal | Neq -> get_equality_binop_type type1 type2 se1 se2 op
	| And | Or -> get_logical_binop_type se1 se2 op (type1, type2)
	| Less | Leq | Greater | Geq -> get_comparison_binop_type type1 type2 se1 se2 op
	| Add | Mult | Sub | Div -> get_arithmetic_binop_type se1 se2 op (type1, type2)
	| _ -> raise (Exceptions.InvalidBinopExpression ((Utils.string_of_op op) ^ " is not a supported binary op"))

and num_lit x = function
		Int_lit(x) -> SInt_lit(x)
	| Float_lit(x) -> SFloat_lit(x)

let check_fdecl func =
	List.iter check_not_void_func_vars func.formals;
	report_duplicate_func (List.map snd func.formals);
	List.iter check_not_void_func_vars func.locals;
	report_duplicate_func (List.map snd func.locals);

let built_in_decls = StringMap.add "print_line"
	{
		datatype 	= Void;
		fname 		= "print_line";
		formals 	= [(Int, "x")];
	  locals 		= [];
		body 			= []
	}
in

let function_decls =
	List.fold_left (fun m fd -> StringMap.add fd.fname fd m) built_in_decls functions
in

let function_decl s = try StringMap.find s function_decls
	with Not_Found -> raise(Exceptions.FunctionNotFound(s))
in

(* Program entry point *)
let check (globals, functions) =
	let _ = function_decl "main" in
	let reserved = add_reserved_functions in
	check_var_decls globals;
	List.iter check_fdecl functions;
	let sast = convert_fdecl_to_sfdecl reserved fname fdecl in sast*)
