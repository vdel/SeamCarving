(* Couleur *)
val cl_button_face_enabled : Graphics.color
val cl_button_face_disabled : Graphics.color
val cl_button_light : Graphics.color
val cl_button_shadow : Graphics.color
val cl_text_disabled : Graphics.color

(* Types *)
type label
type panel
type button
type edit
type environnement

(* crée un nouvel environnement *)
val new_env : unit -> environnement

(* créé un nouvel élément visuel *)
val panel_make : int -> int -> int -> int -> Graphics.color -> panel
val label_make : string -> int -> int -> label
val edit_make : int -> int -> int -> int -> int -> edit
val button_make : string -> int -> int -> int -> int -> bool -> button

(* ajoute un élément visuel à l'environnement *)
val add_button_env : environnement -> button -> unit
val add_edit_env : environnement -> edit -> unit
val add_label_env : environnement -> label -> unit
val add_panel_env : environnement -> panel -> unit
 
(* dessine l'environnement *)
val env_draw : environnement -> unit

(* renvoit le bouton d'id id ou soulève une exception s'il n'existe pas *)
val select_button_by_id : environnement -> int -> button

(* renvoit l'edit d'id id ou soulève une exception s'il n'existe pas *)
val select_edit_by_id : environnement -> int -> edit

(* ouvre la fenetre *)
val open_graph : int -> int -> unit

(* attend le prochain clique puis dispatch l'évènement *)
val click_and_dispatch : environnement -> Image.image -> unit

(* --- Fonction particulières --- *)

(* pour changer la largeur du panel *)
val change_width : panel -> int -> unit
(* pour changer l'événement à déclancher lorsqu'on clique sur le bouton *)
val set_event : button -> (unit -> unit) -> unit
(* pour changer le text du bouton *)
val change_caption : button -> string -> unit
(* pour renvoyer et changer la valeur de l'édit *)
val get_value : edit -> int
val set_value : edit -> int -> unit
(* change le mode: protection ou suppression et renvoit le mode courant *)
val change_mode : environnement -> bool
(* pour quitter la boucle de click_and_dispatch et quitter le programme *)
val close : environnement -> unit
