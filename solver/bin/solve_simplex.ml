let length = List.fold_left (fun acc _ -> 1 + acc) 0;;
let length2 (l : 'a list list) = List.fold_left (fun acc _ -> 1 + acc) 0 l;;

(**
  Function that take a line given 
  @param  i   line selected
  @param  l   matrix
  @return return line i from the matrix l 
**)
let get_line i (l : 'a list list) =
  let rec aux a = function
      [] -> failwith "error get_line : line given isn't right"
    | hd::tl -> if a = i then hd else aux (a+1) tl
  in
  aux 0 l
;;

(**
    Function that return the n-th colon of a matrix
    @param    n indice of the colon to get
    @param    m the matrix to get the colon
    @return give a list of the vec
**)
let get_col n (m : 'a list list) =
  let rec aux i = function
    [] -> failwith "error get_col : n parameter is incorrect"
  | hd::tl -> if i = n then hd else aux (i+1) tl
  in
  List.map (aux 0) m
;;

(**
  Function that return a shorter list
  @param  n   number of element to cut
  @param  l   list
  @return list without the n last element
**)
let rem_last n (l : int list) =
  let len_l = length l in
  let rec aux i = function
      [] -> []
    | hd::tl -> if len_l - i > n then hd :: (aux (i+1) tl) else aux (i+1) tl
  in
  if len_l - n < 0 then failwith "error rem_last : its impossible to delete this much element"
  else
    aux 0 l
;;

(**
  Function that give the indice of the maximum element in the list
  @param   l the list
  @return the indice of the maximum element of the list  
**)
let argmax l =
  let rec aux2 i col max = function
    [] -> col
  | hd::tl -> if hd > max then aux2 (i+1) i hd tl else aux2 (i+1) col max tl
  in
  aux2 0 0 (List.hd l) l
;;

let print_list l =
  List.iter (fun e -> Printf.printf "%d " e) l;
  print_endline ""
;;

let print_mat m =
  List.iter print_list m
;;


let find_arg_coeff_pivot (t : 'a list list) =
  Printf.printf "Matrice : \n";
  print_mat t;
  let height = length2 t in
  let width = length (List.hd t) in
  Printf.printf "height, width of vec : (%d, %d) \n" height width;
  let objective_line = get_line (height-1) t in
  let last_colon = get_col (width-1) t in
  (* Searching for the maximum element of the objective line *)
  let col = argmax (rem_last 1 objective_line) in
  Printf.printf "Objective line : ";
  print_list objective_line;
  Printf.printf "Last colon : ";
  print_list last_colon;
  Printf.printf "Indice de la variable max dans l'objectif : %d\n" col;


