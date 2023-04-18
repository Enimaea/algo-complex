
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

(* Fonction pour trouver la prochaine position vide dans le tableau *)  
  let rec find_next_empty_position board square_size start_pos  =
  let rec find_in_row row col =
    if col + square_size > Array.length board.(0) then
      find_next_empty_position board square_size {x=0; y=row + 1}
    else if row + square_size > Array.length board then None
    else if board.(row).(col) = 0 && board.(row).(col + square_size - 1) = 0 && board.(row + square_size - 1).(col) = 0 && board.(row + square_size - 1).(col + square_size - 1) = 0 then
      Some {x=col; y=row}
    else
      find_in_row row (col + max 1 board.(row).(col))
  in
  find_in_row start_pos.y start_pos.x;;

(* Fonction de récursion pour le pavage *)
  let rec backtrack_paving board bouwkamp_code =
    match bouwkamp_code with
    | [] -> Some board
    | square_size :: remaining_sizes ->
      let rec try_positions pos_opt =
        match pos_opt with
        | None -> None
        | Some pos ->
            place_square board square_size pos;
            match backtrack_paving board remaining_sizes with
            | Some solution_board -> Some solution_board
            | None ->
              remove_square board square_size pos;
              try_positions (find_next_empty_position board square_size {x=pos.x+1; y=pos.y})
      in
      try_positions (find_next_empty_position board square_size {x=0; y=0});;


(* Fonction pour vérifier si un bouwkamp_code est valide *)
  let is_bouwkamp_code_valid n bouwkamp_code =
    let board = Array.make_matrix n n 0 in
    backtrack_paving board bouwkamp_code;;


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


  (* Fonction test_find_square_sum *)
let test_time_backtrack_paving =
  let bouwkamp_code = [18;15;14;10;9;8;7;4;1] in
  let board = Array.make_matrix 32 33 0 in
  let start_time = Unix.gettimeofday () in
  let solution_board_opt = backtrack_paving board bouwkamp_code in
  let end_time = Unix.gettimeofday () in
  let execution_time = end_time -. start_time in
  execution_time;;
