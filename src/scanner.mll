{ open Parser }

rule token = parse
(* Whitespace *)
	[' ' '\t' '\r' '\n'] { token lexbuf }

(* Comments *)
| "/*" { comment lexbuf }

(* Delimiters *)
| '(' { LPAREN }  | ')' { RPAREN } | '{' { LBRACE } | '}' { RBRACE }
| '[' { LBRACKET } | ']' { RBRACKET }

(* Control Flow *)
| "if" { IF } (* | "elseif" { ELSEIF } *) | "else" { ELSE } | "while" { WHILE }
| "for" { FOR } | "return" { RETURN }

(* Conditionals *)
| "==" { EQ } | "!=" { NEQ } | '<' { LT } | ">" { GT }
| "<=" { LEQ } | ">=" { GEQ } | "&&" { AND } | "||" { OR } | '!' { NOT }

(* Arithmetic *)
| '+' { PLUS } | '-' { MINUS } | '*' { TIMES } | '/' { DIVIDE }
| '=' { ASSIGN } | "++" { INC } | "--" { DEC }

(* Types *)
| "int" { INT } | "float" { FLOAT } | "bool" { BOOL } | "void" { VOID }
| "String" { STRING } | "true" { TRUE } | "false" { FALSE }
| "matrix" { MATRIX } | "vector" { VECTOR }

(* Misc. *)
| ';' { SEMI } | ',' { COMMA } | ':' { COLON } | '|' { BAR }
| "rows" { ROWS } | "cols" { COLS } | "len" { LEN } | "tr" { TRANSPOSE } | "new" { NEW } | "free" { FREE }

(* Literals *)
| ['0'-'9']+ as lxm { NUM_LIT(Ast.Int_lit(int_of_string lxm)) }
| ['0'-'9']* '.' ['0'-'9']+ as lxmd { NUM_LIT(Ast.Float_lit(float_of_string lxmd)) }
| '"' (([^ '"'] | "\\\"")* as strlit) '"' { STRING_LIT(strlit) }
| "null" { NULL }

(* Identifiers, EOF *)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^
							  Char.escaped char)) }

and comment = parse
	"*/" { token lexbuf }
| _ { comment lexbuf }
