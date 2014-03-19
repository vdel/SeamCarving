type t;; (*Type d'un pixel *)

(* Création d'un pixel *)
val make_color : int -> int -> int -> t
val make_grey : int -> t

(* écrit un pixel dans le flux de sortie *)
val out : out_channel -> t -> unit

(* Pour afficher avec la libraire Caml *)
val color_of_pixel : t -> Graphics.color

(* renvoit un triplet (r,g,b) à partir de Pixel.t *)
val rgb_of_pixel : t -> int*int*int

(* Calcule l'intensité d'un pixel *)
val intensity : t -> float

(* interpolation linéaire de la couleur des deux pixels en paramètre *)
val interpolation : t -> t -> t
