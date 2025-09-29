%token
COLON EQ GEQ LEQ PLUS
CONS OBJ
INT ID
EOF
MIN MAX

%start <unit> main
%%

main:
|   beg EOF
        { List.iter print_endline $1}

beg:
|   OBJ COLON minmax EQ exp constraints
        { $3 @ $5 @ $6 }

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

minmax:
|   MIN
        { ["min"] }
|   MAX
        { ["max"] }

exp:
|   exp PLUS terme
        { $1 @ ("plus" :: $3)}

terme:
|   ID
    { ["ID"] }
|   INT ID
    { ["INT ID"] }

