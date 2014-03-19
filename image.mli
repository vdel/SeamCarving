(* Type des images *)
type image;;

(* opération sur les images *)
val set_data : image -> int -> int -> Pixel.t -> unit
val get_data : image -> int -> int -> Pixel.t 
val set_intensity : image -> int -> int -> float -> unit
val get_intensity : image -> int -> int -> float 
val set_energy : image -> int -> int -> float -> unit
val get_energy : image -> int -> int -> float
val set_path_energy : image -> int -> int -> Types.path_energy -> unit
val get_path_energy : image -> int -> int -> Types.path_energy 

(* pour récupérer les matrices *)
val get_matrix_energy : image -> Types.energy Matrix.matrix
val get_matrix_path_energy : image -> Types.path_energy Matrix.matrix
val set_matrix_path_energy : image -> Types.path_energy Matrix.matrix -> unit

(* les dimensions *)
val width : image -> int
val height : image -> int
val max_width : image -> int
val max_height : image -> int
val set_max_dims : image -> int -> int -> unit

(* Charger, sauver *)
(* dans un fichier *)
val load : string -> image
val save : image -> string -> unit
(* en mémoire *)
val save_modifications : image -> unit
val cancel_modifications : image -> unit

(* affiche une image *)
val draw : image -> int -> int -> unit

(* Supprime un chemin des matrices de l'image *)
val del_path : image -> Types.path -> unit
(* Ajoute un chemin des matrices de l'image *)
val add_path : image -> Types.path -> unit

(* règle et efface les bonus pour protéger ou supprimer des zones de l'image *)
val set_bonus : image -> int -> int -> int -> float -> unit
val erase_bonus : image -> unit
