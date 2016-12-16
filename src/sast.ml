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
	| SMatrix_lit of sexpr list list * datatype
	| SId of string * datatype
	| SNoexpr
	| SNull
	| SBinop of sexpr * op * sexpr * datatype
	| SUnop of uop * sexpr * datatype
	| SAssign of sexpr * sexpr * datatype
	| SCall of string * sexpr list * datatype
	| SVector_access of string * sexpr * datatype
	| SMatrix_access of string * sexpr * sexpr * datatype
	| SMatrix_row of string * sexpr * datatype
	| SMatrix_col of string * sexpr * datatype
	| SRows of int
	| SCols of int
	| SLen of int

(* Statements *)
type sstmt =
	  SBlock of sstmt list
	| SExpr of sexpr
	| SIf of sexpr * sstmt * sstmt
	| SFor of sexpr * sexpr * sexpr * sstmt
	| SWhile of sexpr * sstmt
	| SReturn of sexpr

(* Function Declarations *)
type sfunc_decl = {
	sreturn_type 	: datatype;
	sfname 		: string;
	sformals 	: formal list;
	slocals  	: local list;
	sbody 		: sstmt list;
}

(* All method declarations | Main entry method *)
type sprogram = var_dec list * func_decl list
