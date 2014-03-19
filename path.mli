(* calcule une matrice contenant les valeurs des énergies des chemins et le met dans la bon champ de l'image *)
val calc : Types.direction -> Image.image -> unit

(* retrouve le chemin d'énergie minimale à partir de la matrice obtenue avec calc *)
val find : Types.direction -> Image.image -> Types.path

(* utilise les fonctions précédentes pour remettre à jour la matrice de calc *)
val update_path : Image.image -> Types.path -> unit

(* dessine le chemin qu'elle va supprimer en rouge pour que l'utilisateur puisse le voir *)
val draw : Image.image -> Types.path -> int -> int -> unit

(* Affiche la matrice des énergies des chemins, pour débugger *)
val draw_mat : Image.image -> int -> int -> unit
