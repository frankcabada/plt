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

(*and check_assign env s e =
	let se1, env = expr_to_sexpr env s in
	let se2, env = expr_to_sexpr env e in
	let type1 = get_type_from_sexpr se1 in
	let type2 = get_type_from_sexpr se2 in
	if type1 = type2
		then SAssign(se1, se2, type1)
		else raise (Exceptions.AssignmentTypeMismatch(Utils.string_of_datatype type1, Utils.string_of_datatype type2))
*)
and check_unop env op e =
	let check_num_unop t = function
			Neg 	-> t
		| 	_ 		-> raise(Exceptions.InvalidUnaryOperation)
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
	|  	Datatype(Bool) 	-> SUnop(op, se, check_bool_unop op)
	| 	_ -> raise(Exceptions.InvalidUnaryOperation)
(*
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
*)
and expr_to_sexpr env = function
	(*	Num_lit i           -> SNum_lit(i), env*)
	|   Bool_lit b       	-> SBool_lit(b), env
	|   String_lit s        -> SString_lit(s), env
	|   Id s                -> SId(s, get_ID_type env s), env
	|   Null                -> SNull, env
	|   Noexpr              -> SNoexpr, env
(*	|   Call(s, el)         -> check_call_type env false env s el, env *)
(*	|   Assign(s, e2)      	-> check_assign env s e2, env *)
	|   Unop(op, e)         -> check_unop env op e, env
(*	|   Binop(e1, op, e2)   -> check_binop env e1 op e2, env *)

and get_type_from_sexpr = function
		SNum_lit(_)					-> Datatype(Int)
	| 	SBool_lit(_)				-> Datatype(Bool)
	| 	SString_lit(_) 				-> Datatype(String)
	| 	SId(_, d) 					-> d
	| 	SBinop(_, _, _, d) 			-> d
	| 	SAssign(_, _, d) 			-> d
	| 	SNoexpr 					-> Datatype(Void)
(*	| 	SCall(_, _, d, _)			-> d*)
	|  	SUnop(_, _, d) 				-> d
	| 	SNull						-> Datatype(Void)

let get_arithmetic_binop_type se1 se2 op = function
				(Datatype(Int), Datatype(Float))
		| 	(Datatype(Float), Datatype(Int))
		| 	(Datatype(Float), Datatype(Float)) 	-> SBinop(se1, op, se2, Datatype(Float))

		| 	(Datatype(Int), Datatype(Int)) 		-> SBinop(se1, op, se2, Datatype(Int))

		| _ -> raise (Exceptions.InvalidBinopExpression "Arithmetic operators don't support these types")

let add_reserved_functions = 
	let reserved_stub name return_type formals = 
		{
			sprimitives		= return_type;
			sfname 			= name;
			sformals 		= formals;
			slocals			= [];
			sbody 			= [];
		}
	in
	let void_t = Datatype(Void) in
	let str_t = Datatype(String) in
	let mf t n = Formal(t, n) in (* Make formal *)
	let reserved = [
		reserved_stub "print_line" 	(void_t) 	([mf str_t "print"]);
	] in
	reserved
