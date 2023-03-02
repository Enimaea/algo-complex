let rec reverse_list lst =
  match lst with
  | [] -> []
  | hd :: tl -> reverse_list tl @ [hd];;

(* fonction pour vérifier si un entier est un carré parfait *)
let is_perfect_square n =
  let s = int_of_float (sqrt (float_of_int n)) in
  s * s = n;;

let get_squares n =
  let rec helper i acc =
    if i > n then acc
    else helper (i+1) (i*i :: acc)
  in
  helper 1 [];;

let rec find_square_sum n squares =
  match squares with
  | [] -> []
  | x :: xs ->
    let remaining = n - x in
    if remaining = 0 then [x]
    else match find_square_sum remaining xs with
         | [] -> find_square_sum n xs
         | ys -> x :: ys;;

let find_sum_of_squares n =
  let squares = get_squares n in
  let rec helper squares acc =
    match squares with
    | [] -> List.rev acc
    | x :: xs ->
      let solution = find_square_sum (n*n - x) xs in
      if solution <> [] then helper xs ((x :: solution) :: acc)
      else helper xs acc
  in
  helper squares [];;

let square_root_list_of_list lst =
  List.map (fun sub_lst -> List.map (fun x -> sqrt (float_of_int x)) sub_lst) lst;;


let a = square_root_list_of_list(find_sum_of_squares 112);;

List.iter (fun l -> List.iter (fun x -> print_float x; print_string " ") l; print_newline ()) a;;
