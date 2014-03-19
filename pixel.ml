(* Type des pixels: gris ou en couleur *)
type t = Grey of int | Color of int*int*int;;

(* Abstraction du type pixel *)
(* Création d'un pixel *)
let make_color r g b=
  Color(r,g,b)
and make_grey c =
  Grey c
;;

(* écrit un pixel dans le flux de sortie *)
let out out_ch pix =
  match pix with
    | Color(r,g,b) ->
	begin
	  output_byte out_ch r;
	  output_byte out_ch g;
	  output_byte out_ch b
	end
    | Grey(c) -> output_byte out_ch c
;;

(* Pour afficher avec la libraire Caml *)
let color_of_pixel pix = 
  match pix with
    | Grey c -> Graphics.rgb c c c
    | Color(r,g,b) -> Graphics.rgb r g b
;;

(* renvoit un triplet (r,g,b) à partir de Pixel.t *)
let rgb_of_pixel pix=
  match pix with
    | Grey c -> (c,c,c)
    | Color(r,g,b) -> (r,g,b)
;;
    
(* Calcule l'intensité d'un pixel *)
let intensity pix = 
  match pix with
    | Grey n -> float_of_int (n)
    | Color (r,g,b) -> (82.0*.float_of_int(r) +. 135.0*.float_of_int(g) +. 38.0*.float_of_int(b))/.255.0 
;;

(* renvoit la couleur moyenne de celle de pix1 et pix2 *)
let interpolation pix1 pix2=
  match pix1,pix2 with
    | (Grey c1, Grey c2) -> Grey ((c1+c2)/2)
    | (Color(r1,g1,b1),Color(r2,g2,b2)) -> Color((r1+r2)/2,(g1+g2)/2,(b1+b2)/2)
    | _ -> failwith ("Pixel.ml: Format couleur et gris incompatibles.\n");
;;
