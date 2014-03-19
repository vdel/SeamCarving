(* D�finition des types fondamentaux *)

(* Energie d'un pixel *)
type energy = float*float*float;;
let (energy_null:energy) = (0.0,0.0,0.0);;  (* Pour initialiser les matrices *)

(* Energie d'un chemin *)
type path_energy = float ;;
let (path_energy_null:path_energy)= 0.0;; (* Pour initialiser les matrices *)
let (path_energy_inf: path_energy)= infinity;;


(* Direction *)
type direction = V | H;;  (* verticale ou horizontale *)


(* Type d'un chemin:
   Un chemin peut �tre vertical (on le parcourt en partant de l'ordonn�e 0 (de bas en haut)) ou horizontal (parcourt de gauche � droite (abscisse 0 � gauche)).  *)  
type turn = L | F | R;; (*Direction � emprunter pour trouver le prochain pixel du chemin dans le sens de parcourt: left, forward, right*)
type path = 
    | VPath of  int*(turn list) (* la colonne du premier pixel et la liste des directions *)
    | HPath of int*(turn list) (* la ligne du premier pixel et la liste des directions *)
;;

(* Malus appliqu� � l'�nergie d'un chemin pour ne pas ajouter toujours le m�me *)
let malus_path = 400.0;;

(* rayon de la brosse pour prot�ger ou effacer *)
let radius = 12;;

(* bonus d'�nergie pour prot�ger ou supprimer *)
let bonus = 10000.0;;
