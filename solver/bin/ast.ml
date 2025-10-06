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
