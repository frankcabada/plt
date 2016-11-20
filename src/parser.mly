%{ open Ast %}

/* Delimiters */
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET

/* Control Flow */
%token IF /*ELSEIF*/ ELSE WHILE FOR RETURN BREAK

/* Conditionals */
%token EQ NEQ LT GT LEQ GEQ AND OR NOT

/* Arithmetic */
%token PLUS MINUS TIMES DIVIDE ASSIGN INC DEC

/* Types */
%token INT BOOL VOID STRING FLOAT TRUE FALSE MATRIX

/* Misc */
%token SEMI COMMA COLON

/* Literals, identifiers, EOF */
%token <Ast.num> NUM_LIT
%token <string> ID
%token <string> STRING_LIT
%token NULL
/*%token CONST*/
%token EOF

/* Precedence and associativity of each operator */
%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS INC DEC /* ?? Correct precedence */
%left TIMES DIVIDE
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
    decls EOF { $1 }

decls:
    /* nothing */      { [], [] }
  | decls gdecl        { ($2 :: fst $1), snd $1 }
  | decls fdecl        { fst $1, ($2 :: snd $1) }

fdecl:
  datatype ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
    { { return_type = $1; fname = $2; formals = $4;
      locals = List.rev $7; body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    datatype ID                   { [Formal($1,$2)] }
  | formal_list COMMA datatype ID { Formal($3, $4) :: $1 }

datatype:
  primitives { Datatype($1) }

primitives:
    INT                                                         { Int }
  | BOOL                                                        { Bool }
  | VOID                                                        { Void }
  | FLOAT                                                       { Float }
  | STRING                                                      { String }
  | MATRIX primitives LBRACKET NUM_LIT RBRACKET                 { Vector($2, $4) }
  | MATRIX primitives LBRACKET NUM_LIT COMMA NUM_LIT RBRACKET   { Matrix($2, $4, $6) }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    datatype ID SEMI { Local($1, $2) }

gdecl:
    datatype ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                                               { Expr $1 }
  | RETURN SEMI                                             { Return Noexpr }
  | BREAK SEMI                                              { Break }
  | RETURN expr SEMI                                        { Return $2 }
  | LBRACE stmt_list RBRACE                                 { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE                 { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt                    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt                           { While($3, $5) }

expr:
    NUM_LIT                                                  { Num_lit($1) }
  | STRING_LIT                                               { String_lit($1) }
  | TRUE                                                     { Bool_lit(true) }
  | FALSE                                                    { Bool_lit(false) }
  | NULL                                                     { Null }
  | ID                                                       { Id($1) }
  /*| CONST primitives ID                                      { Const($2, Id($3)) }*/
  | expr PLUS expr                                           { Binop($1, Add, $3) }
  | expr MINUS expr                                          { Binop($1, Sub, $3) }
  | expr TIMES expr                                          { Binop($1, Mult, $3) }
  | expr DIVIDE expr                                         { Binop($1, Div, $3) }
  | expr EQ expr                                             { Binop($1, Equal, $3) }
  | expr NEQ expr                                            { Binop($1, Neq, $3) }
  | expr LT expr                                             { Binop($1, Less, $3) }
  | expr LEQ expr                                            { Binop($1, Leq, $3) }
  | expr GT expr                                             { Binop($1, Greater, $3) }
  | expr GEQ expr                                            { Binop($1, Geq, $3) }
  | expr AND expr                                            { Binop($1, And, $3) }
  | expr OR expr                                             { Binop($1, Or, $3) }
  | MINUS expr %prec NEG                                     { Unop(Neg, $2) }
  | NOT expr                                                 { Unop(Not, $2) }
  | INC expr                                                 { Unop(Inc, $2) } /* only prefix increment */
  | DEC expr                                                 { Unop(Dec, $2) } /* only prefix decrement */
  | ID ASSIGN expr                                           { Assign($1, $3) }
  | LPAREN expr RPAREN                                       { $2 }
  | ID LPAREN actuals_opt RPAREN                             { Call($1, $3) }
  | LBRACKET expr COLON expr COLON expr RBRACKET             { Mat_init($2, $4, $6) }
  | LBRACKET actuals_opt RBRACKET                            { Matrix_lit($2) }
  | ID LBRACKET expr COMMA expr RBRACKET                     { Matrix_access($1, $3, $5) }
  | ID LBRACKET expr COMMA COLON RBRACKET                    { Matrix_row($1, $3) } /* ?? that's all */
  | ID LBRACKET COLON COMMA expr RBRACKET                    { Matrix_col($1, $5) } /* ?? that's all */

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                         { [$1] }
  | actuals_list COMMA expr      { $3 :: $1 }
  | actuals_list COMMA expr SEMI { $3 :: $1 }