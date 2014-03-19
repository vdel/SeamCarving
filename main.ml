(* supprime n lignes ou colonnes et affiche (x0,y0) *)
let del_path img n d x0 y0 show=  (* show permet d'activer ou non l'affichage en temps réel *)  
  Energy.calc 2 img; (* calcul la matrice d'énergie (et d'intensité) *)
  Path.calc d img;   (* calcul la matrice du chemin *)
  for i = 1 to n do
    begin        
      let p= Path.find d img in          (* trouve le chemin d'énergie minimale, *)
	Image.del_path img p;	         (* le supprime, *)
	Energy.update_del_path 2 img p;  (* met a jour l'énergie *)
	Path.update_path img p;          (* le poids des chemins *)
	if show then
	  begin
	    Image.draw img x0 y0;	 (* affiche l'image *)
	    Path.draw img p x0 y0;       (* affiche le chemin *)
	    Graphics.synchronize ();     (* affiche tout à l'écran *)
	  end
    end
  done;
  Image.draw img x0 y0;      (* affiche le résultat *)
  Graphics.synchronize ();   (* met à jour l'écran *)
;;

(* ajoute n lignes ou colonnes, pareil que la fonction ci-dessus *)
let add_path img n d x0 y0 show= 
  Energy.calc 2 img; 
  Path.calc d img;
  for i = 1 to n do
    begin
      let p= Path.find d img in	
	Image.add_path img p;	
	Energy.update_add_path 2 img p;		
	Path.update_path img p;	
	if show then
	  begin
	    Image.draw img x0 y0;
	    Path.draw img p x0 y0;
	    Graphics.synchronize ()
	  end;
    end
  done;
  Image.draw img x0 y0;
  Graphics.synchronize ();
;;

(* redimensionne l'image *)
let resize img new_w new_h x0 y0 show= (* show permet d'activer ou non l'affichage en temps réel *)  
  let diff_V = new_w-(Image.width img)  (* nombre de colonnes à supprimer/ajouter *)
  and diff_H = new_h-(Image.height img) (* nombre de lignes à supprimer/ajouter *)
  in
    if diff_V<0 then
      del_path img (0-diff_V) Types.V x0 y0 true
    else if diff_V>0 then
	add_path img diff_V Types.V x0 y0 true;
    if diff_H<0 then
      del_path img (0-diff_H) Types.H x0 y0 true
    else if diff_H>0 then
      add_path img diff_H Types.H x0 y0 true
;;

let main _ =
  let nbr_arg =  Array.length Sys.argv in
    if nbr_arg = 1 || Sys.argv.(1)="-help" || Sys.argv.(1)="--help" then
      begin
	print_string "\nLa syntaxe est la suivante:";
	print_string "  resize_img fichier_source fichier_cible\n"
      end
    else
      let img = Image.load Sys.argv.(1) in
	Interface.open_graph 1 1; 
	Graphics.auto_synchronize false; 
	let menu = Interface.new_env () in
	  
	(* ajout des composants graphiques *)	  
	let panel = Interface.panel_make 0 0 525 100 Interface.cl_button_face_enabled in
	  Interface.add_panel_env menu panel;
	  Interface.add_label_env menu (Interface.label_make "Nouvelles dimensions:" 10 55);
	  Interface.add_label_env menu (Interface.label_make "Largeur:" 10 40);
	  Interface.add_label_env menu (Interface.label_make "Hauteur:" 70 40);
	  Interface.add_edit_env menu (Interface.edit_make (Image.width img) 10 15 50 20);
	  Interface.add_edit_env menu (Interface.edit_make (Image.height img) 70 15 50 20);      
	  Interface.add_label_env menu (Interface.label_make "Mode actif:" 160 60);
	  Interface.add_label_env menu (Interface.label_make "(cliquer pour changer)" 160 50);
	  Interface.add_button_env menu (Interface.button_make "Mode protection" 160 10 120 25 true);      
	  Interface.add_button_env menu (Interface.button_make "Redimensionner" 310 45 110 25 true);
	  Interface.add_button_env menu (Interface.button_make "Annuler" 310 10 110 25 true);
	  Interface.add_button_env menu (Interface.button_make "Quitter" 430 10 80 25 true);
	  let enabled = (nbr_arg = 3) in
	    Interface.add_button_env menu (Interface.button_make "Sauver" 430 45 80 25 enabled);
	    
	    (* redimensionne la fenêtre en w*h, affiche éventuellement le menu *)
	    let resize_win w h affich_menu=  
	      Graphics.resize_window w h;
	      if affich_menu then
		begin
		  Interface.change_width panel w;
		  Graphics.set_color Graphics.black;
		  Graphics.fill_rect 0 0 w h;
		  Interface.env_draw menu;	       
		end;
	      Image.draw img 0 (if affich_menu then 100 else 0);
	      Graphics.synchronize();
	    in
	      (* ajout des évenements *)
	    let exit_button () =  (* on quitte *)
	      Interface.close menu
	    in
	      Interface.set_event (Interface.select_button_by_id menu 3) exit_button;
	      let save_button () =  (* on sauve *)
		Image.save img Sys.argv.(2)
	      in
		Interface.set_event (Interface.select_button_by_id menu 4) save_button;
		let mode_button () = (* on change le mode *)
		  if Interface.change_mode menu then
		    Interface.change_caption (Interface.select_button_by_id menu 0) "Mode protection"
		  else
		    Interface.change_caption (Interface.select_button_by_id menu 0) "Mode suppression"
		in
		  Interface.set_event (Interface.select_button_by_id menu 0) mode_button;
		  let resize_button ()=  (* on lance le redimenssionnement *)
		    let nw = (Interface.get_value (Interface.select_edit_by_id menu 0))
		    and nh =  (Interface.get_value (Interface.select_edit_by_id menu 1))	           
		    in    
		      (* Met à jour la fenêtre *)
		      resize_win (max nw (Image.width img)) (max nh (Image.height img)) false;
		      (* Prépare le redimensionnement *)
		      Image.save_modifications img;
		      Image.set_max_dims img nw nh;
		      (* redimensionne *)
		      resize img nw nh 0 0 true;
		      (* ajuste Ã  la nouvelle taille *)
		      resize_win (max nw 525) (nh+100) true;
		      (* remet à zéro les éventuels bonus appliqué *)
		      Image.erase_bonus img;
		  in
		    Interface.set_event (Interface.select_button_by_id menu 1) resize_button;
		    let cancel_button() =  (* on remet l'image d'avant (non modifiée) à l'écran *)
		      Graphics.set_color Graphics.black;
		      Graphics.fill_rect 0 0 (max (Image.width img) 525) (Image.height img+100);
		      Image.cancel_modifications img;
		      Interface.set_value (Interface.select_edit_by_id menu 0) (Image.width img);
		      Interface.set_value (Interface.select_edit_by_id menu 1) (Image.height img);
		      resize_win (max (Image.width img) 525) (Image.height img+100) true
		    in
		      Interface.set_event (Interface.select_button_by_id menu 2) cancel_button;   
		      
		      (* on prépare la fenêtre, on affiche le menu *)
		      resize_win (max 525 (Image.width img)) (Image.height img+100) true;
		      (* et c'est parti ! *)
		      Interface.click_and_dispatch menu img;
;;

main ();;
