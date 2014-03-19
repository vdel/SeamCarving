type t;; (*Type d'un pixel *)

(* Cr�ation d'un pixel *)
val make_color : int -> int -> int -> t
val make_grey : int -> t

(* �crit un pixel dans le flux de sortie *)
val out : out_channel -> t -> unit

(* Pour afficher avec la libraire Caml *)
val color_of_pixel : t -> Graphics.color

(* renvoit un triplet (r,g,b) � partir de Pixel.t *)
val rgb_of_pixel : t -> int*int*int

(* Calcule l'intensit� d'un pixel *)
val intensity : t -> float

(* interpolation lin�aire de la couleur des deux pixels en param�tre *)
val interpolation : t -> t -> t
