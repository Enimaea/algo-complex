# Sum of Squares and Bouwkamp Code Solver

Ce projet contient des fonctions OCaml pour résoudre les problèmes liés à la somme des carrés parfaits et aux codes Bouwkamp pour le pavage de rectangles.

## Fonctionnalités

1. Trouver les combinaisons de carrés parfaits dont la somme est égale à n
2. Trouver le nombre de combinaisons de carrés parfaits dont la somme est égale à n
3. Tester et enregistrer les résultats de la fonction `find_square_sum` dans un fichier CSV
4. Résoudre des problèmes de pavage de rectangles à l'aide de codes Bouwkamp
5. Sauvegarder les solutions de pavage sous forme de fichiers texte et d'images PPM

## Exemples de commandes

### Trouver les combinaisons de carrés parfaits dont la somme est égale à n

let n = 90;;
let solutions = find_sum_of_squares n;;
let num_solutions = find_num_sol_squares n;;

### Tester et enregistrer les résultats de la fonction find_square_sum dans un fichier CSV

let max_n = 80;;
let output_file = "results.csv";;
test_find_square_sum max_n output_file;;

### Résoudre des problèmes de pavage de rectangles à l'aide de codes Bouwkamp

let bouwkamp_code = [18;15;14;10;9;8;7;4;1];;
let board = Array.make_matrix 32 33 0;;
let solution_board_opt = backtrack_paving board bouwkamp_code;;

### Sauvegarder les solutions de pavage sous forme de fichiers texte et d'images PPM

let filename = "solution.txt";;
write_solution_to_file filename solution_board_opt;;

let filename = "solution.ppm";;
let cell_size = 1;; (* Choisissez la taille de la cellule *)
write_solution_to_ppm_file filename solution_board_opt cell_size;;


## il y a deja des demo fait vous aver just a les lancer
vous pouvez compiler chaque démo individuellement en utilisant la commande **make <demo>**, par exemple *make demo1*. Pour compiler toutes les démos, utilisez simplement **make all**. Pour nettoyer les fichiers générés, utilisez **make clean**. Enfin, pour exécuter toutes les démos après les avoir compilées, utilisez **make run**.

si vous voulez compiler et exécuter demo1.ml, vous pouvez faire ce qui suit:
        make demo1
        ./demo1

### demo1

retourn tout les solution pour n = 50.

### demo2

compt le nombre de resultat et le temp d'execution de n = 1 a n = 5
et les met dans un fichier csv **resultsdemo.csv**.

### demo3

retourn le temp d'execution et la matrice resultat pour un rectongle 33*32 avec le code de bouwkamp [18;15;14;10;9;8;7;4;1].

### demo4

genere deux fichier .txt et .ppm ou se trouve le resultat pour un rectongle 33 X 32 avec code de bouwkamp [18;15;14;10;9;8;7;4;1] et un carré 112 X 112 avec le code de bouwkamp [50;35;27;8;19;15;17;11;6;24;29;25;9;1;7;18;16;42;4;37;33]

### demo5

retourn le temp d'execution et la matrice resultat pour un rectongle 33*32 avec le code de bouwkamp [18;15;14;10;9;8;7;4;1] et en etulisent la version ameliorer.

### demo6

genere deux fichier .txt et .ppm ou se trouve le resultat pour un rectongle 33 X 32 avec code de bouwkamp [18;15;14;10;9;8;7;4;1] et en etulisent la version ameliorer.