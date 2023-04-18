
open Printf;;
open Unix;;  

(* Définition du type de position *)
  type position = {x: int; y: int};;


(* Fonction pour placer un carré sur le tableau *)
  let place_square board square_size pos =
    for y = pos.y to pos.y + square_size - 1 do
      for x = pos.x to pos.x + square_size - 1 do
        board.(y).(x) <- square_size
      done
    done;;

(* Fonction pour retirer un carré du tableau *)
  let remove_square board square_size pos =
    for y = pos.y to pos.y + square_size - 1 do
      for x = pos.x to pos.x + square_size - 1 do
        board.(y).(x) <- 0
      done
    done;;

(* Fonction qui verifier si il n y a pas deja un cube dans la zone *)
let is_zone_clear board top_left bottom_right clear_value =
  let rec check_rows y =
    if y > bottom_right.y then
      true
    else
      let rec check_columns x =
        if x > bottom_right.x then
          true
        else if board.(y).(x) = clear_value then
          check_columns (x + 1)
        else
          false
      in
      if check_columns top_left.x then
        check_rows (y + 1)
      else
        false
  in
  check_rows top_left.y;;
    

(* Fonction pour trouver la prochaine position vide dans le tableau *)  
let rec find_next_empty_position board square_size start_pos  =
let rec find_in_row row col =
      if col = Array.length board.(0) then
        find_in_row (row+1) (0)
      else if col + square_size > Array.length board.(0) then None
      else if row + square_size > Array.length board then None
      else if is_zone_clear board {x=col; y=row} {x=col + square_size - 1; y=row + square_size - 1} 0 then
        Some {x=col; y=row}
      else 
        find_in_row row (col + max 1 board.(row).(col))
in
find_in_row start_pos.y start_pos.x;;

(*fonction de pavage iterer une seul fois*)
  let single_iteration_paving board bouwkamp_code =
    let rec try_placing_square square_sizes =
      match square_sizes with
      | [] -> Some board
      | square_size :: remaining_sizes ->
        let pos_opt = find_next_empty_position board square_size {x=0; y=0} in
        match pos_opt with
        | None -> None
        | Some pos ->
            place_square board square_size pos;
            let result = try_placing_square remaining_sizes in
            (match result with
            | None -> remove_square board square_size pos; None
            | _ -> result)
    in
    try_placing_square bouwkamp_code;;

    (*fonction de pavage qui test tout les permutation d'un bouwkamp_code*)
let rec backtrack_paving_ameliorer board bouwkamp_code_permutations =
  match bouwkamp_code_permutations with
  | [] -> None
  | bouwkamp_code :: remaining_permutations ->
    let solution_board_opt = single_iteration_paving board bouwkamp_code in
    match solution_board_opt with
    | Some solution_board -> Some solution_board
    | None -> backtrack_paving_ameliorer board remaining_permutations;;

  
let rec insert x lst =
  match lst with
  | [] -> [[x]]
  | h::t -> 
    (x::lst) :: (List.map (fun el -> h::el) (insert x t));;

let rec sub_list lst n m =
  let rec aux acc i = function
    | [] -> List.rev acc
    | h::t ->
      if i > m then List.rev acc
      else if i >= n then aux (h::acc) (i+1) t
      else aux acc (i+1) t
  in
  aux [] 0 lst;;

(*fonction qui retourn une list de tout les paemutation d'une list de la position n a m*)
let rec perm lst n m =
  let sub_lst = sub_list lst n m in
  match sub_lst with
  | [] -> [sub_lst]
  | h::t -> 
    List.flatten (List.map (insert h) (perm t 0 (m - n - 1)));;

(*fonction qui calcule le factoriel*)
let rec factorial n =
  if n <= 1 then
    1
  else
    factorial (n-1) * n;;


(*fonction pour contourner le probleme de memoire*)
let find_solution_in_batches board bouwkamp_code batch_size =
  let k = List.length bouwkamp_code in
  let total_permutations = factorial k in
  let start_idx = ref 0 in
  let found_solution = ref None in
  while !found_solution = None && !start_idx < total_permutations do
    let end_idx = min (!start_idx + batch_size - 1) (total_permutations - 1) in
    let perms = perm bouwkamp_code !start_idx end_idx in
    let res = backtrack_paving_ameliorer board perms in
    let test_permutations () =
    if res <> None then (
      found_solution := res;
      raise Exit
    )in
    (try test_permutations () with Exit -> ());

    start_idx := end_idx + 1;
  done;

  (!found_solution);;



(* Fonction pour convertir une solution en chaîne de caractères *)
  let solution_to_string solution_board =
    let n = Array.length solution_board and k=Array.length solution_board.(0) in
    let str = ref "" in
    for y = 0 to n - 1 do
      for x = 0 to k - 1 do
        str := !str ^ Printf.sprintf "%4d" solution_board.(y).(x)
      done;
      str := !str ^ "\n"
    done;
    !str;;


(* Fonction pour écrire une solution dans un fichier *)
  let write_solution_to_file filename solution_board_opt =
    match solution_board_opt with
    | None ->
      Printf.printf "No solution found. Not writing to a file.\n"
    | Some solution_board ->
      let str = solution_to_string solution_board in
      let oc = open_out filename in
      output_string oc str;
      close_out oc;
      Printf.printf "Solution written to file: %s\n" filename;;


(* Fonction pour deteminer la couleur *)    
  let color_map size =
    let r = (size * 31) mod 256 in
    let g = (size * 67) mod 256 in
    let b = (size * 123) mod 256 in
    Printf.sprintf "%d %d %d" r g b;;


(* Fonction pour écrire une solution dans un fichier *)
  let solution_to_ppm solution_board cell_size =
    let n = Array.length solution_board and k=Array.length solution_board.(0) in
    let width = k * cell_size in
    let height = n * cell_size in
    let header = Printf.sprintf "P3\n%d %d\n255\n" width height in
    let pixels = ref "" in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let cell_x = x / cell_size in
        let cell_y = y / cell_size in
        let color = color_map solution_board.(cell_y).(cell_x) in
        pixels := !pixels ^ color ^ " "
      done;
      pixels := !pixels ^ "\n"
    done;
    header ^ !pixels;;

(* Fonction pour écrire une solution au format PPM dans un fichier *)
  let write_solution_to_ppm_file filename solution_board_opt cell_size =
    match solution_board_opt with
    | None ->
      Printf.printf "No solution found. Not writing to a file.\n"
    | Some solution_board ->
      let ppm = solution_to_ppm solution_board cell_size in
      let oc = open_out filename in
      output_string oc ppm;
      close_out oc;
      Printf.printf "Solution written to file: %s\n" filename;;



let bouwkamp_code = [18;15;14;10;9;8;7;4;1];;
let board = Array.make_matrix 32 33 0;;

let dlist = find_solution_in_batches board bouwkamp_code 200;;

let filename = "solution3.txt";;
write_solution_to_file filename dlist;;

let filename = "solution3.ppm";;
let cell_size = 1;; (* Choisissez la taille de la cellule *)
write_solution_to_ppm_file filename dlist cell_size;;