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

(* Fonction pour trouver le nombre de combinaisons de carrés parfaits dont la somme est égale à n *)
let rec find_num_square_sum n squares =
  match squares with
  | [] -> 0
  | x :: xs ->
    let remaining = n - x in
    if remaining < 0 then 0
    else if remaining = 0 then 1
    else find_num_square_sum remaining xs + find_num_square_sum n xs;;

let find_sum_of_squares n =
  let squares = get_squares (n * n) in
  find_square_sum (n * n) squares;;

let find_num_sol_squares n =
  let squares = get_squares (n * n) in
  find_num_square_sum (n * n) squares;;

let square_root_list_of_list lst =
  List.map (fun sub_lst -> List.map (fun x -> sqrt (float_of_int x)) sub_lst) lst;;

(* Fonction pour écrire les résultats dans un fichier CSV *)
let write_csv filename results =
  let oc = open_out filename in
  fprintf oc "Number,Results Count,Execution Time (s)\n";
  List.iter (fun (num, count, time) ->
    fprintf oc "%d,%d,%f\n" num count time
  ) results;
  close_out oc;;

(* Fonction pour tester find_square_sum de 1 à n et enregistrer les résultats dans un fichier CSV *)
let test_find_square_sum n output_file =
  let results = ref [] in
  for i = 1 to n do
    let start_time = Unix.gettimeofday () in
    let result_count = find_num_sol_squares i in
    let end_time = Unix.gettimeofday () in
    let execution_time = end_time -. start_time in
    results := (i, result_count, execution_time) :: !results;
  done;
  write_csv output_file (List.rev !results);;

(* Appeler la fonction test_find_square_sum avec la valeur de n et le nom du fichier CSV *)
let () = test_find_square_sum 130 "results3.csv";;