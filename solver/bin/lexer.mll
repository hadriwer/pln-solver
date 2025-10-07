{
    open Parser
}

let digit = ['0'-'9']
let cons = ['c''C']['o''O']['n''N']['s''S']
let obj = ['o''O']['b''B']['j''J']
let max = ['m''M']['a''A']['x''X']
let min = ['m''M']['i''I']['n''N']
let id = ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']*

rule token = parse
    |   obj                         { OBJ }
    |   cons                        { CONS }

    |   max                         { MAX }
    |   min                         { MIN }

    |   "+"                         { PLUS }
    |   "-"                         { MOINS }
    |   "*"                         { FOIS }

    |   "="                         { EQ }
    |   "<="    |   "=<"            { LEQ }
    |   ">="    |   "=>"            { GEQ }                   

    |   digit+ as n                 { INT (float_of_string n) }
    |   id as c                     { ID (c)}

    |   '\n'                        { Lexing.new_line lexbuf; token lexbuf }
    |   ['\t'' ']                   { token lexbuf }
    |   eof                         { EOF }
    |   _   as c                    { Printf.eprintf "Unexpected char : \"%c\"\n" c ; token lexbuf }