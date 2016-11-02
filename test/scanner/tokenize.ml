open Nullparser

let stringify = function
  (* Punctuation *)
  | LPAREN -> "LPAREN"  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"  | RBRACE -> "RBRACE"
  | COMMA -> "COMMA"

  (* Arithmetic Operators *)
  | PLUS -> "PLUS"     | MINUS -> "MINUS"
  | TIMES -> "TIMES"   | DIVIDE -> "DIVIDE"

  (* Relational Operators *)
  | EQ -> "EQ"    | NEQ -> "NEQ"
  | LEQ -> "LEQ"  | GEQ -> "GEQ"

  (* Logical Operators & Keywords *)
  | AND -> "AND"   | OR -> "OR"
  | NOT -> "NOT"

  (* Assignment Operator *)
  | ASSIGN -> "ASSIGN"

  (* Conditional Operators *)
  | IF -> "IF"
  | ELSE -> "ELSE"
  | ELSIF -> "ELSIF"

  (* End-of-File *)
  | EOF -> "EOF"

  (* Identifiers *)
  | ID(string) -> "ID"

  | SEMI -> "SEMI"  | LBRACKET -> "LBRACKET" | RBRACKET -> "RBRACKET"
  | LT -> "LT" | GT -> "GT" | INC -> "INC" | DEC -> "DEC"
  | COLON -> "COLON" | FOR -> "FOR" | WHILE -> "WHILE" | BREAK -> "BREAK"
  | RETURN -> "RETURN" | MAIN -> "MAIN"
  | TRUE -> "TRUE" | FALSE -> "FALSE"
  | INT -> "INT" | BOOL -> "BOOL" | VOID -> "VOID" | FLOAT -> "FLOAT"
  | STRING -> "STRING" | NULL -> "NULL" | INT_LIT(int) -> "INT_LIT"
  | STRING_LIT(string) -> "STRING_LIT" | FLOAT_LIT(float) -> "FLOAT_LIT"
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let rec print_tokens = function
    | EOF -> " "
    | token ->
      print_endline (stringify token);
      print_tokens (Scanner.token lexbuf) in
  print_tokens (Scanner.token lexbuf)
