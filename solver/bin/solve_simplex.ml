let length = Array.fold_left (fun acc _ -> 1 + acc) 0;;
let length2 (l : 'a array array) = Array.fold_left (fun acc _ -> 1 + acc) 0 l;;
let cmp_max = fun a b -> a > b;;
let cmp_min = fun a b -> a < b;;

(**
  Function that return a shorter list
  @param  n   number of element to cut
  @param  l   list
  @return list without the n last element
**)
let rem_last n (t : 'a array) =
  Array.sub t 0 (n+1)
;;

(**
  Function that give you argmin oor argmax of an array
  @param   cmp function that comparing element of the array
  @param   t the array
  @return   the optimum of the array based on the cmp given 
**)
let arg_extreme cmp (t : 'a array) =
  let len = Array.length t in
  if len = 0 then invalid_arg "arg_extreme: empty array";
  let rec aux i best_i best_val =
    if i = len then best_i
    else
      let v = t.(i) in
      if cmp v best_val then aux (i+1) i v
      else aux (i+1) best_i best_val
  in
  aux 1 0 t.(0)
;;

(**
    Function that print 2D array
    @param m the array
**)
let print_mat m =
  let print_list l =
    Array.iter (fun e -> Printf.printf "%f " e) l;
    print_endline ""
  in
  Array.iter print_list m
;;

(**
    Function that find the coord of the shift point
    @param t array to find shift point
    @param height of the matrix
    @param width of the matrix
    @return return coord of the shift point
**)
let find_arg_coeff_pivot (t : float array array) height width =
  let objective_line = t.(height-1) in
  let col = arg_extreme cmp_max (rem_last 1 objective_line) in
  let rec aux i best_i best_val =
    if i >= height - 1 then best_i
    else
      let curr = t.(i).(col) in
      if curr > 0. then
        let calc = t.(i).(width-1) /. curr in
        if calc < best_val then aux (i+1) i calc
        else aux (i+1) best_i best_val
      else
        aux (i+1) best_i best_val
  in
  let ligne = aux 0 (-1) infinity in
  if ligne = -1 then failwith "No limit problem"
  else (ligne, col)

(**
    Function that making the simplex algorithm
    @param tab array of constraints and objective
    @param height of the matrix
    @param width of the matrix
    @return solve the pln problem
**)
let rec simplex tab height width =
  print_mat tab;
  let objective_line_without_last = tab.(height-1) |> rem_last 1 in
  let tf = Array.exists (fun e -> e > 1e-10) objective_line_without_last in
  if not tf
  then tab.(height-1).(width-1)
  else
    let (x,y) = find_arg_coeff_pivot tab height width in
    Printf.printf "(%d, %d)\n" x y;
    let curr = tab.(x).(y) in
    let tab_x = Array.map (fun e -> e /. curr) tab.(x) in
    let res =
      Array.init height (fun i ->
        if i = x then tab_x
        else
          Array.init width (fun j ->
            tab.(i).(j) -. tab.(i).(y) *. tab_x.(j)))
    in
    simplex res height width


(**
    Function that launch the simplex
    @param t matrix of constraints and objective
**)
let solve_simplex (t : float array array) =
  let height = length2 t in
  let width = length t.(0) in
  let res = simplex t height width in
  Printf.printf "Optimum found for expression on %f\n" res;