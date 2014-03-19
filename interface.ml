open Graphics
(* Definition des composants visuels *)
(* couleurs *)
let cl_button_face_enabled = Graphics.rgb 230 230 230 ;;
let cl_button_face_disabled = Graphics.rgb 230 230 230;;
let cl_button_light = Graphics.rgb 255 255 255;;
let cl_button_shadow = Graphics.rgb 138 138 138;;
let cl_text_disabled = Graphics.rgb 180 180 180;;

(*------------------------ Label -------------------------*)
(* (les chaines de caractère) *)
type label = {
  caption : string;
  x0 : int;   (* coordonnées du point en bas à gauche *)
  y0 : int;
};;

let label_make s x y=
{
  caption=s;
  x0=x;
  y0=y;
};;

let label_draw l =
  Graphics.set_color Graphics.black;
  Graphics.moveto l.x0 l.y0; 
  Graphics.draw_string (l.caption);
  Graphics.synchronize()
;;

(*------------------------ Panel -------------------------*)
(* le panneau gris à l'arrière plan *)
type panel = {
  x0 : int;
  y0 : int;
  color: Graphics.color;
  mutable width: int;
  height: int;
};;

let panel_make x y w h c=
{
  x0=x;
  y0=y;
  width=w;
  height=h;
  color=c;
};;

let panel_draw p =
  Graphics.set_color p.color;
  Graphics.fill_rect p.x0 p.y0 (p.x0+p.width) (p.y0+p.height-1);
  Graphics.synchronize()
;;

(* pour changer la largeur du panel *)
let change_width p w=
  p.width<-w;
  panel_draw p
;;

(*------------------------ Boutton -------------------------*)
type button = {
  mutable id_button : int;  (* pour retrouver un bouton suivant l'id *)
  mutable caption : string; (* text du bouton *)
  mutable enabled : bool;   (* activé ou pas (sert à rien) *)
  mutable clicked : bool;   (* enfoncé ou pas *)
  width : int;
  height : int;
  x0 : int;
  y0 : int;
  mutable event: unit -> unit  (* fonction à appeler si on clique dessus *)
};;

let button_make s x y w h e=
{
  id_button=0;
  caption=s;
  enabled=e;
  clicked=false;
  width=w;
  height=h;
  x0=x;
  y0=y;
  event = (fun () -> ())
};;

let button_draw b=
  (* couleur du bouton, au final, enabled sert à rien mais bon *)
  if b.enabled then
    Graphics.set_color cl_button_face_enabled
  else
    Graphics.set_color cl_button_face_disabled;  
  Graphics.fill_rect b.x0 b.y0 b.width b.height;
  (* dessine les bord *)
  if b.clicked then
    Graphics.set_color cl_button_light
  else
    Graphics.set_color cl_button_shadow;
  Graphics.moveto  (b.x0+b.width) (b.y0+b.height);
  Graphics.lineto (b.x0+b.width) b.y0;
  Graphics.lineto b.x0 b.y0;
  if b.clicked then
    Graphics.set_color cl_button_shadow
  else
    Graphics.set_color cl_button_light;
  Graphics.lineto b.x0 (b.y0+b.height);
  Graphics.lineto (b.x0+b.width) (b.y0+b.height);
  (* dessine le texte *)
  let (w,h)=Graphics.text_size b.caption in
  let offset = if b.clicked then 1 else 0 in
    if b.enabled then Graphics.set_color Graphics.black
    else Graphics.set_color cl_text_disabled;
    Graphics.moveto (b.x0+(b.width-w)/2+offset) (b.y0+(b.height-h)/2+offset);
    Graphics.draw_string b.caption;
    Graphics.synchronize();
;;

(* est-ce que la souris est sur le bouton? *)
let button_is_in_rect b (x,y)=
  b.enabled && b.x0<=x && x<=b.x0+b.width && b.y0<=y && y<=b.y0+b.height
;;

(* gestionnaire de la souris pour le bouton *)
let button_gest b=
  let ok = ref false in
    b.clicked <- true;
    button_draw b;
    while not !ok do
      let status = Graphics.wait_next_event [Graphics.Button_up;Graphics.Mouse_motion] in
	if status.button then
	  begin
	    b.clicked<- button_is_in_rect b (status.mouse_x,status.mouse_y);
	    button_draw b;
	  end
	else
	  begin
	    b.clicked<- false;
	    button_draw b;
	    if button_is_in_rect b (status.mouse_x,status.mouse_y) then 
	      b.event ();
	    ok:=true
	  end
    done
;;

(* si on clique sur un bouton, il prend le contrôle *)
let rec button_dispatch l pos= 
  match l with
    |[] -> false
    |t::q -> if button_is_in_rect t pos then 
	begin
	  button_gest t;
	  true;
	end
      else
	button_dispatch q pos;
;;

(* pour initialiser la fonction appelée quand on clique *)
let set_event b f=
  b.event<-f
;;

(* Change le texte *)
let change_caption b s=
  b.caption<-s;
  button_draw b
;;

(*------------------------ Edit -------------------------*)
type edit = {
  mutable id_edit : int;  (* pour retrouver l'edit selon l'id  *)
  mutable number : int;   (* nombre affiché dans la case *)
  mutable focus : bool;   (* est-ce la case à le control des évènements, dans ce cas, on affiche une petite barre vertical *)
  width : int;
  height : int;
  x0 : int;
  y0 : int;
};;


let edit_make n x y w h=
{
  id_edit=0;
  number=n;
  focus=false;
  width=w;
  height=h;
  x0=x;
  y0=y;
};;

let edit_draw b=
  Graphics.set_color Graphics.white;
  Graphics.fill_rect b.x0 b.y0 b.width b.height;
  Graphics.set_color cl_button_light;
  Graphics.moveto  (b.x0+b.width) (b.y0+b.height);
  Graphics.lineto (b.x0+b.width) b.y0;
  Graphics.lineto b.x0 b.y0;
  Graphics.set_color cl_button_shadow;
  Graphics.lineto b.x0 (b.y0+b.height);
  Graphics.lineto (b.x0+b.width) (b.y0+b.height); 
  let (w,h)=Graphics.text_size (string_of_int b.number) in
    Graphics.set_color Graphics.black;
    Graphics.moveto (b.x0+3) (b.y0+(b.height-h)/2);
    Graphics.draw_string (string_of_int b.number);
    if b.focus then
      begin
	Graphics.moveto  (b.x0+4+w) (b.y0+2);
	Graphics.lineto (b.x0+4+w) (b.y0+b.height-2);
      end;
    Graphics.synchronize();
;;

(* est-ce que la souris est sur l'edit? *)
let edit_is_in_rect b (x,y)= 
  b.x0<=x && x<=b.x0+b.width && b.y0<=y && y<=b.y0+b.height
;;

(* gestionnaire des évennements souris et clavier quand l'edit a le contrôle *)
let edit_gest b=
  let ok = ref false in
    while not !ok do
      let status = Graphics.wait_next_event [Graphics.Button_up] in
	if not status.button then
	  begin
	    if edit_is_in_rect b (status.mouse_x,status.mouse_y) then
	      begin
	      	b.focus<-true;
		edit_draw b;
		while not !ok do
		  let status = Graphics.wait_next_event [Graphics.Button_down;Graphics.Key_pressed] in		    
		    if status.keypressed then
		      begin
			match Char.code status.key with
			  | 8 -> 
			     b.number<- b.number/10;
			      edit_draw b;
			  | _ ->
			      begin
				if b.number<3275 then
				try
				  let i = int_of_string (Char.escaped status.key) in
				    b.number<- b.number*10+i;
				    edit_draw b;
				with
				  | _ -> ()
			      end
		      end;
		    if status.button then
		      begin
			b.focus<-false;
			edit_draw b;			
			ok:=true;
		      end
		done
	      end
	  end
    done;
;;

(* si on clique sur un edit, il prend le controle *)
let rec edit_dispatch l pos= 
  match l with
    |[] -> false
    |t::q -> if edit_is_in_rect t pos then 
	begin
	  edit_gest t;
	  true;
	end
      else
	edit_dispatch q pos;
;;

(* envoit la valeur stockée dans l'edit *)
let get_value e =
  e.number
;;

(* change la valeur stockée *)
let set_value e v=
  e.number <- v;
  edit_draw e;
;;

(*------------------------ Environnement -------------------------*)
(* l'enveloppe pour gérer tous les éléments visuels *) 
type environnement= {
  mutable buttons : button list;
  mutable id_buttons : int;
  mutable edits : edit list;
  mutable id_edits : int;
  mutable labels : label list;
  mutable panels : panel list;
  mutable ddone: bool;
  mutable protect: bool; (*true: Mode protection, false: Mode suppression *)
  mutable current_event : Graphics.status option;
};;

let new_env () = {buttons =[]; id_buttons=0; edits=[]; id_edits=0; labels=[]; panels=[]; ddone=false; protect= true; current_event=None};;

(* ajoute le bouton b à l'environnement *)
let add_button_env env b = 
  b.id_button <- env.id_buttons;
  env.id_buttons <- env.id_buttons+1;
  env.buttons<- b::env.buttons
;;

(* ajoute l'edit e à l'environnement *)
let add_edit_env env e = 
  e.id_edit <- env.id_edits;
  env.id_edits <- env.id_edits+1;
  env.edits<- e::env.edits
;;

(* ajoute le label l à l'environnement *)
let add_label_env env l = 
  env.labels<- l::env.labels
;;

(* ajoute le panneau p à l'environnement *)
let add_panel_env env p = 
  env.panels<- p::env.panels
;;

(* dessine l'environnement *)
let env_draw env =
  List.iter panel_draw env.panels;
  List.iter label_draw env.labels;
  List.iter button_draw env.buttons;
  List.iter edit_draw env.edits;  
;;

(* appelle le gestionnaire d'événement de l'élément sur lequel on clique *) 
let dispatch e img status =
  let pos=(status.mouse_x,status.mouse_y) in
    if not (button_dispatch e.buttons pos) then  (* essaye avec les boutons *)
      if not (edit_dispatch e.edits pos) then    (* essaye avec les edits *)
	let x=ref (status.mouse_x) and y=ref (status.mouse_y) in  (* sinon, on veut peut-être protéger ou supprimer *)
	let count = ref 0 in
	  if 100<= !y then(* on clique dans la zone graphique *)	    
	    let ddone=ref false in
	      while not !ddone do
		if !count=0 && 100<= !y then
		  begin
		    if e.protect then 
		      Image.set_bonus img !x (!y-100) Types.radius Types.bonus     (*on applique les bonus ou malus si l'utilisateur clique sur l'image *)
		    else
		      Image.set_bonus img !x (!y-100) Types.radius (0.0 -. Types.bonus);
		    Image.draw img 0 100;
		    Graphics.synchronize();
		  end;
		let status = Graphics.wait_next_event [Graphics.Button_up;Graphics.Mouse_motion] in (* on attend que la souris bouge, ou que le bouton se relache *)
		  if not status.button then
		    ddone:=true
		  else
		    begin
		      x:=status.mouse_x;
		      y:=status.mouse_y
		    end;
		  count:= (!count+1);  (* on dessine quand count = 0, ie une fois sur 3 sinon c'est trop lent *)
		  if !count=3 then count:=0;
	      done	    
;;

(* renvoit le bouton d'id id ou soulève une exception s'il n'existe pas *)
let select_button_by_id env id=
  List.find (fun e->e.id_button=id) env.buttons
;;

(* renvoit l'edit d'id id ou soulève une exception s'il n'existe pas *)
let select_edit_by_id env id=
  List.find (fun e->e.id_edit=id) env.edits
;;

(* ouvre la fenetre *)
let open_graph w h =
  Graphics.open_graph "";
  Graphics.resize_window w h
;;

(* attend le prochain clique puis dispatch l'évènement *)
let click_and_dispatch env img=
  while not env.ddone do
    let status = Graphics.wait_next_event [Graphics.Button_down] in dispatch env img status
  done;
  Graphics.close_graph ()
;;

(* change le mode: protection ou suppression et renvoit le mode courant *)
let change_mode env=	  
  env.protect<- not env.protect;
  env.protect;
;;

(* pour quitter la boucle de click_and_dispatch et quitter le programme *)
let close env=
  env.ddone<-true
;;


