let rec reverse_list lst =
  match lst with
  | [] -> []
  | hd :: tl -> reverse_list tl @ [hd];;

(* Fonction pour vérifier si un entier est un carré parfait *)
let is_perfect_square n =
  let s = int_of_float (sqrt (float_of_int n)) in
  s * s = n;;

(* Fonction pour obtenir les carrés parfaits jusqu'à n inclus *)
let get_squares n =
  let rec helper i acc =
    if i*i > n then acc
    else helper (i+1) (acc @ [i*i])
  in
  helper 1 [];;

(* Fonction pour trouver les combinaisons de carrés parfaits dont la somme est égale à n *)
let rec find_square_sum n squares =
  match squares with
  | [] -> []
  | x :: xs ->
    let remaining = n - x in
    if remaining < 0 then []
    else if remaining = 0 then [[x]]
    else let solutions = find_square_sum remaining xs in
         if solutions <> [] then
           List.map (fun sol -> x :: sol) solutions @ find_square_sum n xs
         else
           find_square_sum n xs;;

let find_sum_of_squares n =
  let squares = get_squares (n * n) in
  find_square_sum (n * n) squares;;

let square_root_list_of_list lst =
  List.map (fun sub_lst -> List.map (fun x -> sqrt (float_of_int x)) sub_lst) lst;;

let a = square_root_list_of_list(find_sum_of_squares 120);;

List.iter (fun l -> List.iter (fun x -> print_float x; print_string " ") l; print_newline ()) a;;