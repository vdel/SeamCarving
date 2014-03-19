(* calcul l'énergie de tout les pixels de la matrice *)
val calc : int -> Image.image -> unit

(* recalcul seulement sur les pixels adjacents au chemin *)
val update_del_path : int -> Image.image -> Types.path -> unit
val update_add_path : int -> Image.image -> Types.path -> unit

(* dessine l'energie *)
val draw : Image.image -> int -> int -> unit

