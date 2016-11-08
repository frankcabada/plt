open Ast
open Printf

(* Binary operators *)
let txt_of_op = function
  (* Arithmetic *)
  | Add -> "Add"
  | Sub -> "Sub"
  | Mult -> "Mult"
  | Div -> "Div"
  (* Boolean *)
  | Equal -> "Equal"
  | Neq -> "Neq"
  | Less -> "Less"
  | Leq -> "Leq"
  | Greater -> "Greater"
  | Geq -> "Geq"
  | And -> "And"
  | Or -> "Or"

(* Unary operators *)
let txt_of_uop = function
  | Not -> "Not"
  | Neg -> "Neg"
  | Inc -> "Inc"
  | Dec -> "Dec"

(* Primitives *)
let rec txt_of_primitives = function
  | Int -> "Int"
  | Bool -> "Bool"
  | Void -> "Void"
  | String -> "String"
  | Float -> "Float"
  | Vector(x, i) -> sprintf "Vector(%s, %s)" (txt_of_primitives x) (string_of_int i)
  | Matrix(x, i, j) -> sprintf "Matrix(%s, %s, %s)" (txt_of_primitives x) (string_of_int i) (string_of_int j)

(* Expressions *)
let rec txt_of_expr = function
  | Int_lit(x) -> sprintf "Int_lit(%s)" (string_of_int x)
  | Bool_lit(x) -> sprintf "Bool_lit(%s)" (string_of_bool x)
  | Id(x) -> sprintf "Id(%s)" x
  | Noexpr -> sprintf ""
  | Binop(e1, op, e2) -> sprintf "Binop(%s, %s, %s)"
      (txt_of_expr e1) (txt_of_op op) (txt_of_expr e2)
  | Unop(op, e) -> sprintf "Unop(%s, %s)" (txt_of_uop op) (txt_of_expr e)
  | Assign(x, e) -> sprintf "Assign(%s, %s)" x (txt_of_expr e)
  | Call(x, args) -> sprintf "Call(%s, [%s])" x (txt_of_list args)
  | Mat_init(e1, e2, e3) -> sprintf "Mat_init(%s,%s,%s)" (txt_of_expr e1) (txt_of_expr e2) (txt_of_expr e3)
  | String_lit(x) -> sprintf "String_lit(%s)" x
  | Float_lit(x) -> sprintf "Float_lit(%s)" (string_of_float x)
  | Null -> sprintf "Null"
  | Matrix_lit(args) -> sprintf "Matrix_lit([%s])" (txt_of_list args)
  | Matrix_access(x,e1,e2) -> sprintf "Matrix_access(%s,%s,%s)" x (txt_of_expr e1) (txt_of_expr e2)
  | Matrix_row(x,e) -> sprintf "Matrix_row(%s,%s)" x (txt_of_expr e)
  | Matrix_col(x,e) -> sprintf "Matrix_col(%s,%s)" x (txt_of_expr e)

(* Lists *)
and txt_of_list = function
  | [] -> ""
  | [x] -> txt_of_expr x
  | _ as l -> String.concat " ; " (List.map txt_of_expr l)

(* Statements *)
let rec txt_of_stmt = function
  | Block(args) -> sprintf "Block([%s])" (txt_of_st_list args)
  | Expr(e) -> sprintf "Expr(%s)" (txt_of_expr e)
  | If(e, s1, s2) -> sprintf "If(%s,%s,%s)" (txt_of_expr e) (txt_of_stmt s1) (txt_of_stmt s2)
  | Else(s) -> sprintf "Else(%s)" (txt_of_stmt s)
  | For(e1, e2, e3, s) -> sprintf "For(%s,%s,%s,%s)" (txt_of_expr e1) (txt_of_expr e2) (txt_of_expr e3) (txt_of_stmt s)
  | While(e,s) -> sprintf "While(%s,%s)" (txt_of_expr e) (txt_of_stmt s)
  | Return(e) -> sprintf "Return(%s)" (txt_of_expr e)
  | Break(e) -> sprintf "Break(%s)" (txt_of_expr e)

and txt_of_st_list = function
  | [] -> ""
  | [x] -> txt_of_stmt x
  | _ as l -> String.concat " ; " (List.map txt_of_stmt l)

(* Function declarations
and txt_of_fdecl f =
  sprintf "Fdecl({ primitives=%s ; fname=%s ; formals=[%s] ; locals=[%s] ; body=[%s] })"
    (txt_of_primitives f.primitives) (f.fname) *)

(* Expressions *)
(* let txt_of_num = function
  | Num_int(x) -> string_of_int x

(* Function declarations *)
and txt_of_fdecl f =
  sprintf "Fdecl({ params=[%s] ; body=%s ; return = %s })"
    (String.concat " ; " f.params) (txt_of_stmts f.body) (txt_of_expr f.return)

(* Lists *)
and txt_of_list = function
  | [] -> ""
  | [x] -> txt_of_expr x
  | _ as l -> String.concat " ; " (List.map txt_of_expr l)

(* Statements *)
and txt_of_stmt = function
  | Do(expr) -> sprintf "Do(%s)" (txt_of_expr expr)

and txt_of_stmts stmts =
  let rec aux acc = function
      | [] -> sprintf "[%s]" (String.concat " ; " (List.rev acc))
      | stmt :: tl -> aux (txt_of_stmt stmt :: acc) tl
  in aux [] stmts
*)
(* Program entry point *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let result = txt_of_stmt program in
    print_endline result
