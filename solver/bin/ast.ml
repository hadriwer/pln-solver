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
    
let make_simplex_table tree  =
  (*===================================================================*)
  (* Var to optimize and number variables int the problems *)
  let (var_opti, n_var) = cnt_number_variable tree in
  (* Number of constraints in the problem *)
  let n_cons = cnt_number_constr tree in
  Printf.printf "Var to optimize = %s, Number var = %d, Number of contraint = %d\n" var_opti n_var n_cons;

  (*===================================================================*)
  (* Create a Hashtbl to store var *)
  let tbl = Hashtbl.create n_var in
  (* Length = number of contraints + 1 (for the objective line) *)
  let idx = ref 0 in
  let register_var x =
    if not (Hashtbl.mem tbl x) then (
      Hashtbl.add tbl x !idx;
      incr idx
    )
  in

  let get_idx x =
    if not (Hashtbl.mem tbl x) then failwith "Semantic error : var isn't in the objective"
    else Hashtbl.find tbl x
  in

  (*===================================================================*)
  let n_cols = 2 * (n_var - 1) + 1 in
  let table = Array.make_matrix (n_cons + 1) (n_cols) 0. in

  let rec fill_tbl = function
      | NONE -> ()
      | INT _ -> ()
      | ID x -> register_var x;
      | BINOP (_, e1, e2) -> (fill_tbl e1; fill_tbl e2)
      | EXPR (_, _, e2) -> 
        (
          fill_tbl e2;
        )
  in
  let rec fill_expr row = function
      | NONE -> ()
      | BINOP (_, INT(a), ID(x))
      | BINOP (_, ID(x), INT(a)) ->
        (
          let idx = get_idx x in
          table.(row).(idx) <- a;
          
          )
      | BINOP (_, e1, e2) -> (fill_expr row e1; fill_expr row e2) 
      | EXPR (e1, _, INT(i))
      | EXPR (INT(i), _, e1) ->
        (
          table.(row).(n_cols - 1) <- i;
          fill_expr row e1;
        )
      | EXPR (_, _, e) ->
        (
          fill_expr row e;
        )
      | _ -> ()
  in
  let rec fill_constr row = function
        NOP -> ()
      | CONSTR (e, c1) -> fill_expr row e; fill_constr (row+1) c1
  in
  match tree with
    OBJ (_, e, c) -> (
      fill_tbl e;
      fill_expr n_cons e;
      fill_constr 0 c;
      (* Set the identity matrix *)
      let nb_var_mat = n_var - 1 in
      for i = 0 to  nb_var_mat - 1 do
        table.(i).(i + nb_var_mat) <- 1.;
      done;
      (var_opti, table, tbl)
    )
;;
