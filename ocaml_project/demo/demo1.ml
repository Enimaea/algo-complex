#load "unix.cma";;
open Printf;;
open Unix;;


(* Fonction pour obtenir les carrés parfaits jusqu'à n inclus *)
let get_squares n =
  let rec aux i acc =
    if i*i > n then acc
    else aux (i+1) (acc @ [i*i])
  in
  aux 1 [];;


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


(* Fonction principale  *)
let find_sum_of_squares n =
  let squares = get_squares (n * n) in
  find_square_sum (n * n) squares;;


(* Fonction qui applique la rcine a tous les ensemble de solution *)
let square_root_list_of_list lst =
  List.map (fun sub_lst -> List.map (fun x -> sqrt (float_of_int x)) sub_lst) lst;;



let a = square_root_list_of_list(find_sum_of_squares 50);;

List.iter (fun l -> List.iter (fun x -> print_float x; print_string " ") l; print_newline ()) a;;