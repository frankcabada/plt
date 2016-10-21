{ open Nullparser }

rule token = parse
	[' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
	(* ['n'] { token lexbux n+1 ? }  *)
| "/*" { comment lexbuf } (* Comments *)
| "//" { comment2 lexbuf } (* Single Line Comments *)
| '(' { LPAREN } | '=' { ASSIGN } | "if" { IF }
| ')' { RPAREN } | "==" { EQ } | "else" { ELSE }
| '{' { LBRACE } | "!=" { NEQ } | "for" { FOR }
| '}' { RBRACE } | '<' { LT } | "while" { WHILE }
| ';' { SEMI } | "<=" { LEQ } | "return" { RETURN }
| ',' { COMMA } | ">" { GT } | "int" { INT }
| '+' { PLUS } | ">=" { GEQ } | "bool" { BOOL }
| '-' { MINUS } | "&&" { AND } | "void" { VOID }
| '*' { TIMES } | "||" { OR } | "true" { TRUE }
| '/' { DIVIDE } | '!' { NOT } | "false" { FALSE }
| '[' { LBRACKET } | "++" { INC } | "main" { MAIN }
| ']' { RBRACKET } | "--" { DEC }
| ':' { COLON }
| "null" { NULL }
| "String" { STRING }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^
							  Char.escaped char)) }

and comment = parse
	"*/" { token lexbuf }
| _ { comment lexbuf }

(* ?? *)
and comment2 = parse
	'\n' { token lexbuf}
| _ { comment2 lexbuf }
