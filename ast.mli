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
	(* | Inc
	| Dec *)

(* Types *)
type typ =
	| Int
	| Bool
	| Void
	| String				
	| Float

(* Matrices *)
type matrix = 
	  VectorDec of typ * int * string
	| MatrixDec of typ * int * int * string

(* Bind *)
type bind = typ * string

(* Expressions *)
type expr =
	| Int_lit of int
	| BoolLit of bool
	| Id of string
	| Noexpr
	| Binop of expr * op * expr
	| Unop of uop * expr
	| Assign of string * expr
	| Call of string * expr list
	| Mat_init of int * int * int
	| String_lit of string
	| Float_lit of float

(* Statements *)
type stmt =
	| Block of stmt list
	| Expr of expr
  	| If of expr * stmt * stmt
  	| Else of stmt
  	| For of expr * expr * expr * stmt
  	| While of expr * stmt
	| Return of expr

(* Function Declarations *)
type func_decl = {
	typ 	: typ;
	fname 	: string;
	formals : bind list;
	locals  : bind list;
	body 	: stmt list;
}

(* Start Symbol *)
type program = bind list * func_decl list
