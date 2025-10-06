%{
        open Ast
%}
%token
COLON EQ GEQ LEQ 
PLUS MOINS FOIS
CONS OBJ
EOF
MIN MAX

%token<float> INT
%token<string> ID

%type <tree> beg
%type <constr> constraints inequalities
%type <expr> exp inequality terme
%type <optimum> minmax

%start <tree> main
%%

main:
|   beg EOF
        { $1}

beg:
|   OBJ COLON minmax ID EQ exp constraints
        { OBJ ($3, EXPR (ID($4), COMP_EQ, $6), $7) }

constraints:
|   CONS COLON inequalities
        { $3 }

inequalities:
|   /* empty */
        { NOP }
|   inequality inequalities
        { CONSTR ($1, $2) }

inequality:
|   exp conmparaison INT
        { EXPR ($1, $2, INT($3)) }
|   INT conmparaison exp
        { EXPR ($3, $2, INT($1)) }

conmparaison:
|       GEQ
        { COMP_GE }
|       LEQ
        { COMP_LE }

minmax:
|   MIN
        { MIN }
|   MAX
        { MAX }

exp:
|   exp PLUS terme
        { BINOP (OP_ADD, $1, $3) }
|   exp MOINS terme
        { BINOP (OP_SUB, $1, $3) }
|   terme
        { $1 }

terme:
|   ID
        { BINOP (OP_MUL, INT(1.), ID($1)) }
|   INT ID
        { BINOP (OP_MUL, INT($1), ID($2)) }
|   INT FOIS ID
        { BINOP (OP_MUL, INT($1), ID($3)) }
|   ID FOIS INT
        { BINOP (OP_MUL, INT($3), ID($1)) }

