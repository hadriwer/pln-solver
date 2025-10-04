%token
COLON EQ GEQ LEQ 
PLUS MOINS FOIS
CONS OBJ
EOF
MIN MAX

%token<int> INT
%token<string> ID

%type <string list>
beg constraints exp minmax inequalities inequality terme

%start <unit> main
%%

main:
|   beg EOF
        { List.iter print_endline $1}

beg:
|   OBJ COLON minmax ID EQ exp constraints
        { "objective" :: $3 @ $6 @ $7 }

constraints:
|   CONS COLON inequalities
        { "Constraints" :: $3 }

inequalities:
|   /* empty */
        { [] }
|   inequality inequalities
        { $1 @ $2 }

inequality:
|   exp GEQ INT
        { $1 }
|   exp LEQ INT
        { $1 }
|   INT LEQ exp
        { $3 }
|   INT GEQ exp
        { $3 }

minmax:
|   MIN
        { ["min"] }
|   MAX
        { ["max"] }

exp:
|   exp PLUS terme
        { $1 @ ("plus" :: $3)}
|   exp MOINS terme
        { $1 @ ("moins" :: $3) }
|   terme
        { $1 }

terme:
|   ID
        { ["ID("^$1^")"] }
|   INT ID
        { ["INT("^(string_of_int $1)^") * ID("^$2^")"] }
|   INT FOIS ID
        { ["INT("^(string_of_int $1)^") * ID("^$3^")"] }
|   ID FOIS INT
        { ["INT("^(string_of_int $3)^") * ID("^$1^")"] }

