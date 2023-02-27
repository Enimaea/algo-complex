let rec reverse_list lst =
  match lst with
  | [] -> []
  | hd :: tl -> reverse_list tl @ [hd]

(* fonction pour vérifier si un entier est un carré parfait *)
let is_perfect_square n =
  let s = int_of_float (sqrt (float_of_int n)) in
  s * s = n

(* fonction pour obtenir la liste des carrés parfaits inférieurs ou égaux à n *)
let get_squares n =
  let rec helper i acc =
    if i * i <= n then helper (i+1) (i*i :: acc)
    else acc
  in
  List.rev (helper 1 [])

(* fonction pour trouver une combinaison de carrés distincts qui somme à n *)
let rec find_square_sum n squares =
  match squares with
  | [] -> []
  | x :: xs ->
    let remaining = n - x in
    if remaining = 0 then [x]
    else if remaining < 0 then []
    else match find_square_sum remaining xs with
         | [] -> find_square_sum n xs
         | ys -> x :: ys

(* fonction principale *)
let find_sum_of_squares n =
  let squares = reverse_list (get_squares (n*n)) in
  let rec helper squares =
    match squares with
    | [] -> []
    | x :: xs ->
      let solution = find_square_sum (n*n - x) xs in
      if solution <> [] then x :: solution
      else helper xs
  in
  helper squares;;

  let square_root_list lst =
    List.map sqrt (List.map float_of_int lst);;

square_root_list (find_sum_of_squares 112);;