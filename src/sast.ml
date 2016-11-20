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
	sreturn_type 	: datatype;
	sfname 			: string;
	sformals 		: formal list;
	slocals  		: local list;
	sbody 			: sstmt list;
}

(* All method declarations | Main entry method *)
type sprogram =  {
	var_dec : var_dec list;
	functions : sfunc_decl list;
}
