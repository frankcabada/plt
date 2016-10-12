% { open Ast % }

% token LPAREN RPAREN LBRACE RBRACE SEMI COMMA
% token PLUS MINUS TIMES DIVIDE LBRACKET RBRACKET COLON
% token ASSIGN EQ NEQ LT LEQ GT GEQ AND OR NOT INC DEC CHAR
% token IF ELSE FOR WHILE RETURN MAIN
% token TRUE FALSE
% token FLOAT INT BOOL VOID
% token <int> LITERAL
% token <string> ID
% token EOF

%

% right ASSIGN
% left OR
% left AND
% left EQ NEQ
% left LT GT LEQ GEQ
% left PLUS MINUS
% left TIMES DIVIDE
% right NOT NEG
% start main
% type <int> main

%%

main:
    expr EOL      { $1 }

expr:
    INT { $1 }
  | LPAREN expr RPAREN { $2 }
  | LBRACE expr RBRACE { - $2 }
  | expr PLUS expr { $1 + $3 }
  | expr MINUS expr { $1 - $3 }
  | expr TIMES expr { $1 * $3 }
  | expr DIV expr { $1 / $3 }

typedef:
    ID {
        match $1 with
        | "int" -> int
        | "bool" -> bool
        | "void" -> void
        | "float" -> float
    }
