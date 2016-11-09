open Ast

(* Data types *)
type data_type = 
	| Int
	| Float
	| String
	| Bool
	| Void
	| Matrix of data_type
	| Func of func

and func = {
	param_types: data_type list;
	return_type: data_type;
}

type var = {
	name: string;
	mutable s_type: data_type;
	builtin: bool;
}

(* Expressions *)
type expr_wrapper =
	| Expr of expr * data_type

and expr = 
	| Num_lit of Ast.num
	| String_lit of string
	| Bool_lit of bool
	| Void_lit
	| Uop of Ast.uop * expr_wrapper
	| Binop of expr_wrapper * Ast.binop * expr_wrapper
	| Id of string
	| Assign of string * expr_wrapper
	| Call of expr_wrapper * expr_wrapper list
	| Func_decl of func_decl
	| If of if_decl

and func_decl = {
	f_name: string;
	params: string list;
	body: stmt list;
	return: expr_wrapper;
}

and if_decl = {
	c_name: string;
	cond: expr_wrapper;
	stmt_1: expr_wrapper;
	stmt_2: expr_wrapper;
}

and stmt = 
	| Do of expr_wrapper

type program = stmt list