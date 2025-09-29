let path : string = "bin/main.plns"


let () = 
  let ic = open_in path in
  let lexbuf = Lexing.from_channel ic in
  try
    Parser.main Lexer.token lexbuf;
    print_endline "OK";
  with
    | Parser.Error ->
        let pos = lexbuf.Lexing.lex_curr_p in
        Printf.eprintf "Parser error at line %d, column %d\n"
            pos.Lexing.pos_lnum
            (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
        print_endline "NO";