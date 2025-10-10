type cmp =
| COMP_NE     (*NOT EQUAL*)
| COMP_EQ     (*EQUAL*)
| COMP_LT     (*LESSER THAN*)
| COMP_LE     (*LESSER EQUAL*)
| COMP_GT     (*GREATER THAN*)
| COMP_GE     (*GREATER EQUAL*)
;;

type binop =
| OP_ADD
| OP_SUB
| OP_MUL
;;

type expr =
| NONE
| INT of float
| ID of string
| BINOP of binop * expr * expr
| EXPR of expr * cmp * expr
;;

type constr =
| NOP
| CONSTR of expr * constr
;;

type optimum =
| MIN
| MAX
;;

type tree =
| OBJ of optimum * expr * constr
;;

let print_tab tab =
  let rec aux tab =
    if tab <= 0 then ()
    else (Printf.printf "  "; aux (tab-1))
  in
  aux tab
;;

let print_ast t =
  let print_optimum tab o =
    print_tab tab;
    match o with
      MAX -> Printf.printf "MAX ";
    | MIN -> Printf.printf "MIN ";
  in
  let print_binop tab b =
    print_tab tab;
    match b with
      OP_ADD -> Printf.printf "OP_ADD "
    | OP_SUB -> Printf.printf "OP_SUB "
    | OP_MUL -> Printf.printf "OP_MUL "
  in
  let print_comp tab c =
    print_tab tab;
    match c with
      COMP_NE -> Printf.printf "COMP_NE ";
    | COMP_EQ -> Printf.printf "COMP_EQ ";
    | COMP_LT -> Printf.printf "COMP_LT ";
    | COMP_LE -> Printf.printf "COMP_LE ";
    | COMP_GT -> Printf.printf "COMP_GT ";
    | COMP_GE -> Printf.printf "COMP_GE ";
  in
  let rec print_exp tab (exp : expr) =
    print_tab tab;
    match exp with
      NONE -> Printf.printf "NONE "
    | INT (f) -> Printf.printf "INT(%f) " f
    | ID (x) -> Printf.printf "ID(%s) " x
    | BINOP (b, e1, e2) -> (
            Printf.printf "BINOP \n";
            print_binop (tab+1) b; Printf.printf "\n";
            print_exp (tab+1) e1; Printf.printf "\n";
            print_exp (tab+1) e2; Printf.printf "\n";
      )
    | EXPR (e1, c, e2) -> (
            Printf.printf "EXPR \n";
            print_exp (tab+1) e1; Printf.printf "\n";
            print_comp (tab+1) c; Printf.printf "\n";
            print_exp (tab+1) e2; Printf.printf "\n";
    )
  in
  let rec print_constr tab c =
    print_tab tab;
    match c with
      NOP -> Printf.printf "NOP ";
    | CONSTR (e, c) -> (
        Printf.printf "CONSTR \n";
        print_exp (tab+1) e; Printf.printf "\n";
        print_constr (tab+1) c; Printf.printf "\n";
    )
  in
  let print_tree tab t =
    print_tab tab;
    match t with
      OBJ (o, e, c) -> (
        Printf.printf "OBJ \n";
        print_optimum (tab+1) o; Printf.printf "\n";
        print_exp (tab+1) e; Printf.printf "\n";
        print_constr (tab+1) c; Printf.printf "\n";
      )
  in
  print_tree 0 t;
;;


let cnt_number_variable t =
  let rec aux exp =
    match exp with
      | NONE -> ("", 0)
      | INT _ -> ("", 0)
      | ID x -> (x, 1)
      | BINOP (_, e1, e2) -> ("", snd (aux e1) + snd (aux e2))
      | EXPR (e1, _, e2) -> 
          let e1 = aux e1 in
          (fst e1, snd e1 + snd (aux e2))
      
  in
  match t with
    OBJ (_, e, _) -> aux e
;;

let cnt_number_constr t =
  let rec aux c =
    match c with
        NOP -> 0
      | CONSTR (_, c1) -> 1 + aux c1
  in
  match t with
    OBJ (_, _, c) ->
      let res = aux c in
      if res = 0 then failwith "No constraint" else res
;;

(* Count artificial var if we get ">=" *)
let cnt_number_artificial t = 
  let aux_expr = function
    | EXPR (_, COMP_GE, _) -> 1
    | _ -> 0
  in
  let rec aux_constr = function
      NOP -> 0
    | CONSTR (e, c) -> aux_expr e + aux_constr c
  in
  match t with
    OBJ (_, _, c) -> aux_constr c
;;


let make_simplex_problem tree =
  let open Lp in
  let rec aux_expr = function
    | INT i -> (c i)
    | ID x -> Lp.var x
    | BINOP (OP_ADD, e1, e2) -> (aux_expr e1) ++ (aux_expr e2)
    | BINOP (OP_MUL, e1, e2) -> (aux_expr e1) *~ (aux_expr e2)
    | BINOP (OP_SUB, e1, e2) -> (aux_expr e1) ++ (c (-1.) *~ (aux_expr e2))
    | EXPR (_, COMP_EQ, e) -> aux_expr e
    | _ -> failwith "error aux_expr : construct ast"
  in
  let rec aux_constr = function
        NOP -> []
      | CONSTR (e, c) ->
        (
          let hd =
            match e with
              | EXPR (e1, COMP_GE, e2) -> (aux_expr e1) >~ (aux_expr e2)
              | EXPR (e1, COMP_LE, e2) -> (aux_expr e1) <~ (aux_expr e2)
              | _ -> failwith "error aux_constr : construct ast"
          in
          hd :: aux_constr c
        )
  in
  let aux_opti = function
      MIN -> minimize
    | MAX -> maximize
  in
  let rec aux_var_index = function
    | ID x -> [(x, Lp.var x)]
    | EXPR (_, COMP_EQ, e2) -> (aux_var_index e2)
    | BINOP (_, e1, e2)
    | EXPR (e1, _, e2) -> (aux_var_index e1) @ (aux_var_index e2)
    | _ -> []
  in
  match tree with
      OBJ (optimum, e, c) -> 
        (
          let var_index = aux_var_index e in
          let problem = 
            make 
            ((aux_opti optimum) (aux_expr e))
            (aux_constr c)
          in
          let var_obj =
            match e with
                EXPR (ID(t), COMP_EQ, _) -> t
              | _ -> failwith "error var_obj : ID doesnt exist."
          in
          (var_index, problem, var_obj)
        )