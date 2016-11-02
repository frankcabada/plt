%{ %}

%token LPAREN RPAREN LBRACE RBRACE SEMI COMMA
%token PLUS MINUS TIMES DIVIDE LBRACKET RBRACKET
%token ASSIGN EQ NEQ LT LEQ GT GEQ AND OR NOT INC DEC COLON
%token IF ELSE ELSIF FOR WHILE RETURN MAIN BREAK
%token TRUE FALSE
%token INT BOOL VOID STRING NULL FLOAT
%token <int> INT_LIT
%token <string> STRING_LIT
%token <float> FLOAT_LIT
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
