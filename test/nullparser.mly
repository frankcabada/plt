%{ %}

%token LPAREN RPAREN LBRACE RBRACE SEMI COMMA
%token PLUS MINUS TIMES DIVIDE LBRACKET RBRACKET
%token ASSIGN EQ NEQ LT LEQ GT GEQ AND OR NOT INC DEC COLON
%token IF ELSE FOR WHILE RETURN MAIN
%token TRUE FALSE
%token INT BOOL VOID STRING NULL
%token <int> LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

%start main
%type <int> main

%%

main: EOF { 0 }

%%
