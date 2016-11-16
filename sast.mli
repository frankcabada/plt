(*
 * COMS4115: Cmat semantically checked abstract syntax tree
 *
 * Authors:
 *  - Marissa Ojeda
 *  - Daniel Rojas
 *  - Mike Berkowitz
 *  - Frank Cabada
 *)

open Ast

type snum =
		SInt_lit of int
	|	SFloat_lit of float

(* Expressions *)
type sexpr =
	  SNum_lit of snum
	| SBool_lit of bool
	| SString_lit of string
	| SMatrix_lit of sexpr list
	| SId of string * datatype
	(*| Const of primitives * expr (* ?? is this correct *)*)
	| SNoexpr
	| SNull
	| SBinop of sexpr * op * sexpr * datatype
	| SUnop of uop * sexpr * datatype
	| SAssign of string * sexpr * datatype
	| SCall of string * sexpr list * datatype
	| SMat_init of sexpr * sexpr * sexpr
	| SMatrix_access of string * sexpr * sexpr
	| SMatrix_row of string * sexpr
	| SMatrix_col of string * sexpr

(* Statements *)
type sstmt =
	  SBlock of sstmt list
	| SExpr of sexpr * datatype
	| SIf of sexpr * sstmt * sstmt
(*  	| Elseif of expr * stmt * stmt *)
	| SElse of sstmt
	| SFor of sexpr * sexpr * sexpr * sstmt
	| SWhile of sexpr * sstmt
	| SReturn of sexpr * datatype
	| SBreak

(* Function Declarations *)
type sfunc_decl = {
	sprimitives 	: datatype;
	sfname 			: string;
	sformals 		: formal list;
	slocals  		: local list;
	sbody 			: sstmt list;
}

type smain_decl = {
	smainlocals	: bind list;
	smainbody	: stmt list;
}

(* All method declarations | Main entry method *)
type sprogram =  {
	functions : sfunc_decl list;
	main : smain_decl;
}

(* Data types
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
*)
