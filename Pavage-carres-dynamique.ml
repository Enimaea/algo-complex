open Printf
open Unix
open Hashtbl
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
let rec find_square_sum cache n squares =
  if Hashtbl.mem cache n then
    Hashtbl.find cache n
  else
    match squares with
    | [] -> []
    | x :: xs ->
      let remaining = n * n - x in
      if remaining < 0 then []
      else if remaining = 0 then [[x]]
      else 
          let solutions = find_square_sum cache remaining  xs in
          if solutions <> [] then
            List.map (fun sol -> x :: sol) solutions @ find_square_sum cache n xs
          else
            find_square_sum cache n xs;;

  let find_sum_of_squares cache n =
    let squares = get_squares (n * n) in
    let result = find_square_sum cache n squares in
    Hashtbl.add cache n result;
    result;;

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
let test_find_square_sum  n output_file =
  let tabl = Hashtbl.create 1000 in
  let results = ref [] in
  for i = 1 to n do
    let start_time = Unix.gettimeofday () in
    let solutions = find_sum_of_squares tabl i in
    let end_time = Unix.gettimeofday () in
    let execution_time = end_time -. start_time in
    let result_count = List.length solutions in
    results := (i, result_count, execution_time) :: !results;
  done;
  write_csv output_file (List.rev !results);;

(* Appeler la fonction test_find_square_sum avec la valeur de n et le nom du fichier CSV *)
let () = test_find_square_sum  10 "results2.csv";;