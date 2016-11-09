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

type num =
	| Int_lit of int
	| Float_lit of float

(* Expressions *)
type expr =
	| Num_lit of num
	| Bool_lit of bool
	| String_lit of string
	| Matrix_lit of expr list
	| Id of string
	(*| Const of primitives * expr (* ?? is this correct *)*)
	| Noexpr
	| Null
	| Binop of expr * op * expr
	| Unop of uop * expr
	| Assign of string * expr
	| Call of string * expr list
	| Mat_init of expr * expr * expr (* Int_lit instead of expr? *)
	| Matrix_access of string * expr * expr (* Int_lit instead of expr? *)
	| Matrix_row of string * expr
	| Matrix_col of string * expr

(* Types *)
type primitives =
	| Int
	| Bool
	| Void
	| String
	| Float
	| Vector of primitives * num
	| Matrix of primitives * num * num

(* Bind *)
type bind = primitives * string

(* Statements *)
type stmt =
	| Block of stmt list
	| Expr of expr
	| If of expr * stmt * stmt
(*  	| Elseif of expr * stmt * stmt *)
	| Else of stmt
	| For of expr * expr * expr * stmt
	| While of expr * stmt
	| Return of expr
	| Break of expr

(* Function Declarations *)
and func_decl = {
	primitives 	: primitives;
	fname 		: string;
	formals 	: bind list;
	locals  	: bind list;
	body 		: stmt list;
}

and main_decl = {
	mainlocals	: bind list;
	mainbody	: stmt list;
}

(* Start Symbol *)
type program = Program of main_decl * func_decl list
