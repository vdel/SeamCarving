(* pixel vide ou un pix_content *)
type 'a pix 

(* un ensemble de pixel ie un curseur initial d'où on pourra se déplacer dans la matrice et sa position dans la "matrice"*)
type 'a matrix 

(* obtient le voisin correspondant *)
val get_left : 'a pix -> 'a pix
val get_right : 'a pix -> 'a pix
val get_up : 'a pix -> 'a pix
val get_down : 'a pix -> 'a pix

(* obtient l'étiquette du pixel *)
val get_val : 'a pix -> 'a

(* modifie l'étiquette *)
val set_val : 'a pix -> 'a -> unit

(* donne le curseur initial de la matrice *)
val get_cursor : 'a matrix -> 'a pix

(* change de place le curseur initial*)
val move_cursor : 'a matrix -> int -> int -> unit

(* récupère ou modifie le pixel en position i,j dans la matrice *)
val get : 'a matrix -> int -> int -> 'a 
val set : 'a matrix -> int -> int -> 'a -> unit 

(* créer une nouvelle matrice: taille * étiquette. Initialisée avec un seul pixel de voisins des pixels vides *)
val make : int -> int -> 'a -> 'a matrix

(* pour créer une matrice à partir d'un pixel donné *)
val explicit_make : 'a pix -> int -> int -> 'a matrix

(* change un réseau de pixel en vraie matrice *)
val array_of_matrix : 'a matrix -> 'a array array

(* idem mais recopie que les zones nécessaires après la suppression d'un chemin *)
val special_array_of_matrix : 'a matrix -> Types.path -> 'a array array

(* copie vraie matrice t dans le réseau de pixels m *)
val copy_array : 'a array array -> 'a matrix -> int -> int -> unit

(* idem qu'avant, ne recopie pas toute la matrice *)
val special_copy_array : 'a array array -> 'a matrix -> Types.path -> int -> int -> unit

(* supprime le chemin *)
val del_path : 'a matrix -> Types.path -> unit

(* ajoute le chemin *)
val add_path : 'a matrix -> Types.path -> unit








