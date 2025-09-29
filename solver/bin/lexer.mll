{
    open Parser
}

let digit = ['0'-'9']
let cons = ['c''C']['o''O']['n''N']['s''S']
let obj = ['o''O']['b''B']['j''J']
let max = ['m''M']['a''A']['x''X']
let min = ['m''M']['i''I']['n''N']
let id = ['a'-'Z']['a'-'Z''0'-'9']*

rule token = parse
    |   obj                         { OBJ }
    |   cons                        { CONS }

    |   max                         { MAX }
    |   min                         { MIN }

    |   ":"                         { COLON }
    |   "="                         { EQ }
    |   "+"                         { PLUS }
    |   "<="                        { LEQ }
    |   ">="                        { GEQ }

    |   digit+                      { INT }
    |   id                          { ID }

    |   eof                         { EOF }
    |   _   as c                    { token lexbuf }