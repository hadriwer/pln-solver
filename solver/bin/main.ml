open Solve_simplex
let path : string = "bin/main.plns";;
let mat =[
  [2; 3; 2; 1; 0; 0; 90];
  [1; 2; 1; 0; 1; 0; 81];
  [4; 3; 1; 0; 0; 1; 120];
  [8; 5; 6; 0; 0; 0; 0]
];;

let () = 
  let ic = open_in path in
  let lexbuf = Lexing.from_channel ic in
  try
    Parser.main Lexer.token lexbuf;
    print_endline "OK";
    find_arg_coeff_pivot mat;
  with
    | Parser.Error ->
        let pos = lexbuf.Lexing.lex_curr_p in
        Printf.eprintf "Parser error at line %d, column %d\n"
            pos.Lexing.pos_lnum
            (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
        print_endline "NO";