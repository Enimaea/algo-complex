  type position = {x: int; y: int};;

  let can_place_square board square_size pos =
    if pos.x + square_size > Array.length board.(0) || pos.y + square_size > Array.length board then
      false
    else
      let rec check_rows y =
        if y = pos.y + square_size then
          true
        else
          let rec check_columns x =
            if x = pos.x + square_size then
              true
            else if board.(y).(x) = 0 then
              check_columns (x + 1)
            else
              false
          in
          if check_columns pos.x then
            check_rows (y + 1)
          else
            false
      in
      check_rows pos.y;;

  let place_square board square_size pos =
    for y = pos.y to pos.y + square_size - 1 do
      for x = pos.x to pos.x + square_size - 1 do
        board.(y).(x) <- square_size
      done
    done;;


  let remove_square board square_size pos =
    for y = pos.y to pos.y + square_size - 1 do
      for x = pos.x to pos.x + square_size - 1 do
        board.(y).(x) <- 0
      done
    done;;
  
  let rec find_next_empty_position board start_pos  =
    let rec find_in_row row col =
      if col = Array.length board.(0) then
        find_next_empty_position board {x=0; y=row + 1} 
      else if board.(row).(col) = 0 then
        Some {x=col; y=row}
      else
        find_in_row row (col + 1)
    in
    if start_pos.y = Array.length board then None
    else find_in_row start_pos.y start_pos.x;;
  
  let rec backtrack_paving board bouwkamp_code =
    match bouwkamp_code with
    | [] -> Some board
    | square_size :: remaining_sizes ->
      let rec try_positions pos_opt =
        match pos_opt with
        | None -> None
        | Some pos ->
          if can_place_square board square_size pos then (
            place_square board square_size pos;
            match backtrack_paving board remaining_sizes with
            | Some solution_board -> Some solution_board
            | None ->
              remove_square board square_size pos;
              try_positions (find_next_empty_position board {x=pos.x+1; y=pos.y})
          ) else
            try_positions (find_next_empty_position board {x=pos.x+1; y=pos.y})
      in
      try_positions (find_next_empty_position board {x=0; y=0});;

  let is_bouwkamp_code_valid n bouwkamp_code =
    let board = Array.make_matrix n n 0 in
    backtrack_paving board bouwkamp_code;;

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

  let color_map size =
    let r = (size * 31) mod 256 in
    let g = (size * 67) mod 256 in
    let b = (size * 123) mod 256 in
    Printf.sprintf "%d %d %d" r g b;;

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
  let solution_board_opt = backtrack_paving board bouwkamp_code;;

  let filename = "solution.txt";;
  write_solution_to_file filename solution_board_opt;;

  let filename = "solution.ppm";;
  let cell_size = 1;; (* Choisissez la taille de la cellule *)
  write_solution_to_ppm_file filename solution_board_opt cell_size;;


  let bouwkamp_code = [50;42;37;35;33;29;27;25;24;19;18;17;16;15;11;9;8;7;6;4;2];;
  let board = Array.make_matrix 112 112 0;;
  let solution_board_opt = backtrack_paving board bouwkamp_code;;

  