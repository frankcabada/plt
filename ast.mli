(* Binary Operators *)
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or

(* Unary Operators *)
type uop = Neg | Not | Inc | Dec

(* Nums *)
type num = Int_lit of int | Float_lit of float

(* Types *)
type primitives = Int	| Bool	| Void	| String	| Float
	| Vector of primitives * num
	| Matrix of primitives * num * num

type datatype = Datatype of primitives

(* Bind *)
type bind = primitives * string

type formal = Formal of datatype*string
type local = Local of datatype*string
type var_dec = datatype*string

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
	| Mat_init of expr * expr * expr
	| Matrix_access of string * expr * expr
	| Matrix_row of string * expr
	| Matrix_col of string * expr

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
	| Break

(* Function Declarations *)
type func_decl = {
	primitives 	: datatype;
	fname 		: string;
	formals 	: formal list;
	locals  	: local list;
	body 		: stmt list;
}

(* Start Symbol *)
type program = var_dec list * func_decl list
