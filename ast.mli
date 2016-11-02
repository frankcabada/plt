type op = Add | Sub | Mult | Div | Equal | 
		  Neq | Less | Leq | Greater | Geq | And | Or (* | COLON *)
type uop = Neg | Not (* | INC of typ | DEC of typ *)
type typ = Int | Bool | Void (* | Null | MAT of typ * int * int do a check when formed *) 
type bind = typ * string

type expr = Literal of int 					| BoolLit of bool 
		  | Id of string 					| Noexpr
		  | Binop of expr * op * expr		| Unop of uop * expr
		  | Assign of string * expr			| Call of string * expr list
		  | String of string 				| Double of float


type stmt = Block of stmt list				| Expr of expr
		  | If of expr * stmt * stmt
		  | ELSE of stmt (* ? *)
		  | For of expr * expr * expr * stmt
		  | While of expr * stmt			| Return of expr

type func_decl = {
	typ 	: typ;
	fname 	: string;
	formals : bind list;
	locals  : bind list;
	body 	: stmt list;
}

type program = bind list * func_decl list
