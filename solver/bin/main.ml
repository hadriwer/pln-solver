open Ast

let write problem = Lp.write "my_problem.lp" problem

let solve problem l var_obj =
  (* For other interfaces, use Lp_glpk_js or Lp_gurobi instead *)
  match Lp_glpk.solve problem with
  | Ok (obj, xs) ->
      Printf.printf "Objective %s = %.2f\n" var_obj obj ;
      List.iter (fun e -> Printf.printf "%s: %.2f " (fst e) (Lp.PMap.find (snd e) xs)) l;
      print_endline "";
  | Error msg ->
      print_endline msg

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
    (* print_ast tree; *)
    let (var_index, problem, var_obj) = make_simplex_problem tree in
    if Lp.validate problem 
    then (write problem; solve problem var_index var_obj;)
    else
      print_endline "Oops, my problem is broken.";
  with
    | Parser.Error ->
        let pos = lexbuf.Lexing.lex_curr_p in
        Printf.eprintf "Parser error at line %d, column %d\n"
            pos.Lexing.pos_lnum
            (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
        print_endline "Syntax error";
