
let length = List.fold_left (fun acc e -> 1 + acc) 0;;

(* 
  Function that take a line given 
  @param  i   line selected
  @param  l   matrix
  @return return line i from the matrix l 
*)
let get_line i (l : 'a list list) =
  let rec aux a = function
      [] -> failwith "error get_line : line given isn't right"
    | hd::tl -> if a = i then hd else aux (a+1) tl
  in
  aux 0 l
;;

(*
  Function that return a shorter list
  @param  n   number of element to cut
  @param  l   list
  @return list without the n last element
*)
let rem_last n l =
  let len_l = length l in
  let rec aux i = function
      [] -> []
    | hd::tl -> if len_l - i > n then hd :: (aux (i+1) tl) else aux (i+1) tl
  in
  if len_l - n < 0 then failwith "error rem_last : its impossible to delete this much element"
  else
    aux 0 l
;;

(*
  Function that give the indice of the maximum element in the list
  @param   l the list
  @return the indice of the maximum element of the list  
*)
let argmax l =
  let rec aux2 i col max = function
    [] -> col
  | hd::tl -> if hd > max then aux2 (i+1) i hd tl else aux2 (i+1) col max tl
  in
  aux2 0 0 (List.hd l) l
;;


let find_arg_coeff_pivot (t : 'a list list) =
  let len_t = length t in
  let objective_line = get_line len_t t in
  (* Searching for the maximum element of the objective line *)
  let col = argmax (rem_last 1 objective_line) in
  let objectiv

