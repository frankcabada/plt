(* Pretty Printer *)
open Ast
open Sast
open Parser

let save file string =
	 let channel = open_out file in
	 output_string channel string;
	 close_out channel

let replace input output =
	Str.global_replace (Str.regexp_string input) output

(* Print data types *)
let string_of_primitive = function
			Int							-> "int"
	| 	Float						-> "float"
	| 	Void						-> "void"
	| 	Bool						-> "bool"
	| 	String					-> "String"
	| 	Vector(p,i) 		-> "vector"
	| 	Matrix(p,i,j) 	-> "matrix"

let rec print_brackets = function
			1 	-> "[]"
	| 	a -> "[]" ^ print_brackets (a - 1)

let string_of_datatype = function
    	Datatype(p)		-> (string_of_primitive p)

(* Print expressions *)
let string_of_op = function
			Add			-> "+"
	 | 	Sub			-> "-"
	 | 	Mult		-> "*"
	 | 	Div			-> "/"
	 | 	Equal		-> "=="
	 | 	Neq			-> "!="
	 | 	Less		-> "<"
	 | 	Leq			-> "<="
	 | 	Greater	-> ">"
	 | 	Geq			-> ">="
	 | 	And			-> "and"
	 | 	Or			-> "or"

let string_of_uop = function
		Not			-> "not"
	| Inc			-> "++"
	| Dec			-> "--"
	| Neg			-> "-"

let string_of_num = function
		Int_lit(x) -> string_of_int x
	| Float_lit(x) -> string_of_float x

let rec string_of_bracket_expr = function
			[] 						-> ""
	| 	head :: tail 	-> "[" ^ (string_of_expr head) ^ "]" ^ (string_of_bracket_expr tail)

and string_of_expr = function
		Num_lit(i)				-> string_of_num i
	|	Bool_lit(b)				-> if b then "true" else "false"
	|	String_lit(s)			-> "\"" ^ (String.escaped s) ^ "\""
	|	Id(s)							-> s
	|	Binop(e1, o, e2)	-> (string_of_expr e1) ^ " " ^ (string_of_op o) ^ " " ^ (string_of_expr e2)
	|	Assign(s, e)			-> (s) ^ " = " ^ (string_of_expr e)
	|	Noexpr						-> ""
	|	Call(f, el)				-> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
	| Unop(uop, e)				-> (string_of_uop uop) ^ "(" ^ string_of_expr e ^ ")"
	|	Null							-> "null"
;;

let string_of_snum = function
		SInt_lit(x) -> string_of_int x
	|	SFloat_lit(x) -> string_of_float x

let rec string_of_bracket_sexpr = function
			[] 				-> ""
	| 	head :: tail 	-> "[" ^ (string_of_sexpr head) ^ "]" ^ (string_of_bracket_sexpr tail)

and string_of_sarray_primitive = function
		[] 				-> ""
	|   [last]			-> (string_of_sexpr last)
	| 	head :: tail 	-> (string_of_sexpr head) ^ ", " ^ (string_of_sarray_primitive tail)

and string_of_sexpr = function
		SNum_lit(i)							-> string_of_snum i
	|	SBool_lit(b)						-> if b then "true" else "false"
	|	SString_lit(s)					-> "\"" ^ (String.escaped s) ^ "\""
	|	SId(s, _)								-> s
	|	SBinop(e1, o, e2, _)		-> (string_of_sexpr e1) ^ " " ^ (string_of_op o) ^ " " ^ (string_of_sexpr e2)
	|	SAssign(s, e, _)				-> (s) ^ " = " ^ (string_of_sexpr e)
	|	SNoexpr									-> ""
	|	SCall(f, el, _)					-> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
	| SUnop(uop, e, _)				-> (string_of_uop uop) ^ "(" ^ string_of_sexpr e ^ ")"
	|	SNull										-> "null"
;;

let string_of_local_expr = function
		Noexpr -> ""
	|  	e 	   -> " = " ^ string_of_expr e

(* Print statements *)
let rec string_of_stmt indent =
	let indent_string = String.make indent '\t' in
	let get_stmt_string = function
			Block(stmts) 			->
				indent_string ^ "{\n" ^
					String.concat "" (List.map (string_of_stmt (indent+1)) stmts) ^
				indent_string ^ "}\n"

		| 	Expr(expr) 				->
				indent_string ^ string_of_expr expr ^ ";\n";

		| 	Return(expr) 			->
				indent_string ^ "return " ^ string_of_expr expr ^ ";\n";

		| 	If(e, s, Block([Expr(Noexpr)])) 	->
				indent_string ^ "if (" ^ string_of_expr e ^ ")\n" ^
					(string_of_stmt (indent+1) s)

		| 	If(e, s1, s2) 			->
				indent_string ^ "if (" ^ string_of_expr e ^ ")\n" ^
					string_of_stmt (indent+1) s1 ^
				indent_string ^ "else\n" ^
					string_of_stmt (indent+1) s2

		|   Else(s) ->
				indent_string ^ "else\n" ^ string_of_stmt (indent+1) s

		| 	For(e1, e2, e3, s) 		->
				indent_string ^ "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^ string_of_expr e3  ^ ")\n" ^
					string_of_stmt (indent) s

		| 	While(e, s) 			->
				indent_string ^ "while (" ^ string_of_expr e ^ ")\n" ^
					string_of_stmt (indent) s

		|  	Break					-> indent_string ^ "break;\n"
	in get_stmt_string

let string_of_local_sexpr = function
		SNoexpr 	-> ""
	|  	e 	   		-> " = " ^ string_of_sexpr e

let rec string_of_sstmt indent =
	let indent_string = String.make indent '\t' in
	let get_stmt_string = function

			SBlock(stmts) 			->
				indent_string ^ "{\n" ^
					String.concat "" (List.map (string_of_sstmt (indent+1)) stmts) ^
				indent_string ^ "}\n"

		| 	SExpr(expr, _) 				->
				indent_string ^ string_of_sexpr expr ^ ";\n";

		| 	SReturn(expr, _) 			->
				indent_string ^ "return " ^ string_of_sexpr expr ^ ";\n";

		| 	SIf(e, s, SBlock([SExpr(SNoexpr, _)])) 	->
				indent_string ^ "if (" ^ string_of_sexpr e ^ ")\n" ^
					(string_of_sstmt (indent+1) s)

		| 	SIf(e, s1, s2) 			->
				indent_string ^ "if (" ^ string_of_sexpr e ^ ")\n" ^
					string_of_sstmt (indent+1) s1 ^
				indent_string ^ "else\n" ^
					string_of_sstmt (indent+1) s2

		| 	SFor(e1, e2, e3, s) 		->
				indent_string ^ "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^ string_of_sexpr e3  ^ ")\n" ^
					string_of_sstmt (indent) s

		| 	SWhile(e, s) 			->
				indent_string ^ "while (" ^ string_of_sexpr e ^ ")\n" ^
					string_of_sstmt (indent) s

		|  	SBreak					-> indent_string ^ "break;\n"
	in get_stmt_string

(* Print Function *)

let string_of_formal = function
		Formal(d, s) -> (string_of_datatype d) ^ " " ^ s

let string_of_formal_name = function
		Formal(_, s) -> s

let string_of_local = function
	Local(d, s) -> (string_of_datatype d) ^ " " ^ s

let string_of_func_decl fdecl =
	"" ^ (string_of_datatype fdecl.return_type) ^ " " ^ (fdecl.fname) ^ " " ^
	(* Formals *)
	"(" ^ String.concat "," (List.map string_of_formal fdecl.formals) ^ ") {\n" ^
	(* Locals *)
	String.concat "" (List.map string_of_local fdecl.locals) ^
	(* body *)
	String.concat "" (List.map (string_of_stmt 2) fdecl.body) ^
	"\t}\n\n"

let string_of_vdecl = function
	(d, s) -> (string_of_datatype d) ^ " " ^ s ^ ";\n"

(* Print whole program *)
let string_of_program = function
	(vars, fdecls) ->
		String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
		String.concat "\n" (List.map string_of_func_decl fdecls)
