(* Binary Operators *)
type op =
	| Add
	| Sub
	| Mult
	| Div
	| Equal
	| Neq
	| Less
	| Leq
	| Greater
	| Geq
	| And
	| Or

(* Unary Operators *)
type uop =
	| Neg
	| Not
	| Inc
	| Dec

(* Types *)
type primitives =
	| Int
	| Bool
	| Void
	| String				
	| Float
	| Matrix of primitives

(* Bind *)
type bind = primitives * string

(* Expressions *)
type expr =
	| Int_lit of int
	| Bool_lit of bool
	| Id of string
	| Const of string (* ?? is this correct *)
	| Noexpr
	| Binop of expr * op * expr
	| Unop of uop * expr
	| Assign of string * expr
	| Call of string * expr list
	| Mat_init of expr * expr * expr (* Int_lit instead of expr? *)
	| String_lit of string
	| Float_lit of float
	| Null
	| Matrix_lit of expr list
	| Matrix_access of string * expr * expr (* Int_lit instead of expr? *)
	| Matrix_row of string * expr

(* Statements *)
type stmt =
	| Block of stmt list
	| Expr of expr
  	| If of expr * stmt * stmt
(*   	| Elseif of expr * stmt *)
  	| Else of stmt
  	| For of expr * expr * expr * stmt
  	| While of expr * stmt
	| Return of expr
	| Break of expr

(* Function Declarations *)
type func_decl = 
{
	primitives 	: primitives;
	fname 		: string;
	formals 	: bind list;
	locals  	: bind list;
	body 		: stmt list;
}

type main_decl = {
	locals  	: bind list;
	body 		: stmt list;
}

(* Start Symbol *)
type program = Program of main_decl * func_decl list
