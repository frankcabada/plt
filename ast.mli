type op = PLUS | MINUS | TIMES | DIVIDE | EQ | 
		  NEQ | LT | LEQ | GT | GEQ | AND | OR | COLON
type uop = NOT | INC of typ | DEC of typ
type typ = INT | BOOL | VOID | NULL | DOUBLE | STRING | MAT of typ * int * int (* do a check when formed *)
type bind = typ * string

type expr = Literal of int 					| BooLit of bool 
		  | Id of string 					| Noexpr
		  | Binop of expr * op * expr		| Unop of uop * expr
		  | Assign of string * expr			| Call of string * expr list

type stmt = Block of stmt list				| Expr of expr
		  | If of expr * stmt * stmt
		  | ELSE of stmt (* ? *)
		  | For of expr * expr * expr
		  | While of expr * stmt			| Return of expr

type func_decl = {
	typ 	: typ;
	fname 	: string;
	formals : bind list;
	locals  : bind list;
	body 	: stmt list;
}

type program = bind list * func_decl list