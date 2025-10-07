open Solve_simplex
open Ast


let () = 
  if Array.length Sys.argv < 2 then (
    Printf.printf "Usage: %s <input_file>\n" Sys.argv.(0);
    exit 1
  );

  let filename = Sys.argv.(1) in
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in

  try
    let tree = Parser.main Lexer.token lexbuf in
    let (var, tab, _) = make_simplex_table tree in
    let res = solve_simplex tab in
    (* print_ast tree; *)
    Printf.printf "Simplex gives %s = %f\n" var res;
  with
    | Parser.Error ->
        let pos = lexbuf.Lexing.lex_curr_p in
        Printf.eprintf "Parser error at line %d, column %d\n"
            pos.Lexing.pos_lnum
            (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
        print_endline "Syntax error";