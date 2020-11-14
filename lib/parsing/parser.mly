/* Parser */

/* TinyL tokens */
%token <int> DIGIT
%token <string> VAR
%token READ PRINT ASSIGN
%token ADD SUB MUL
%token AND XOR
%token SEMICOLON END
%token EOF

%start <Ast.pgm option> prog
%type  <Ast.stmt> stmt
%type  <Ast.expr> expr
%%

prog:
  | EOF                                      { None }
  | stmt END                       { Some ($1, []) }
  | stmt SEMICOLON stmt_list END   { Some ($1, $3) } ;

stmt:
  | VAR ASSIGN expr               { Assign ($1, $3) }
  | READ VAR                             { Read  $2 }
  | PRINT VAR                            { Print $2 } ;

expr:
  | ADD expr expr                    { Add ($2, $3) }
  | SUB expr expr                    { Sub ($2, $3) }
  | MUL expr expr                    { Mul ($2, $3) } 
  | AND expr expr                    { And ($2, $3) }
  | XOR expr expr                    { Xor ($2, $3) }
  | VAR                                    { Var $1 }
  | DIGIT                                  { Dig $1 } ;

stmt_list:
  s = separated_list(SEMICOLON, stmt)           { s } ;