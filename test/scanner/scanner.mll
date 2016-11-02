{ open Nullparser }

rule token = parse
(* Whitespace *)
	[' ' '\t' '\r' '\n'] { token lexbuf }
	(* ['n'] { token lexbux n+1 ? }  *)

(* Comments *)
| "/*" { comment lexbuf } | "//" { comment2 lexbuf }

(* Delimiters *)
| '(' { LPAREN }  | ')' { RPAREN } | '{' { LBRACE } | '}' { RBRACE }
| '[' { LBRACKET } | ']' { RBRACKET }

(* Control Flow *)
| "if" { IF } | "elsif" { ELSIF } | "else" { ELSE } | "while" { WHILE } | "for" { FOR }
| "return" { RETURN } | "main" { MAIN } | "break" { BREAK }

(* Conditionals *)
| "==" { EQ } | "!=" { NEQ } | '<' { LT } | ">" { GT }
| "<=" { LEQ } | ">=" { GEQ } | "&&" { AND } | "||" { OR } | '!' { NOT }

(* Arithmetic *)
| '+' { PLUS } | '-' { MINUS } | '*' { TIMES } | '/' { DIVIDE }
| '=' { ASSIGN } | "++" { INC } | "--" { DEC }

(* Types *)
| "int" { INT } | "double" { DOUBLE } | "bool" { BOOL } | "void" { VOID }
| "null" { NULL } | "String" { STRING } | "true" { TRUE } | "false" { FALSE }

(* Misc. *)
| ';' { SEMI } | ',' { COMMA } | ':' { COLON }

(* Literals, Identifiers, EOF *)
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
