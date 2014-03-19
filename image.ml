(* Type Image *)
type image =
{
  (* image originale: permet d'annuler les modfications *)
  color : bool; (* image en couleur ou pas *)
  mutable width : int; (* largeur originale *)
  mutable height : int;(* hauteur originale *)
  mutable data : Pixel.t Matrix.matrix; (* Matrice des pixels originale *)

  (* image modifiée *)
  mutable mod_width : int;                               (* Nouvelles dimensions *)
  mutable mod_height : int;
  mutable mod_data : Pixel.t Matrix.matrix;              (* Matrice des pixels *)
  mutable intensity : float Matrix.matrix;               (* Matrice de l'intensité des pixels; pour optimiser le calcul de l'énergie *)
  mutable energy : Types.energy Matrix.matrix;           (* Energie des pixels *)
  mutable path_energy: Types.path_energy Matrix.matrix;  (* Energie des chemins (programmation dynamique) *)  
  mutable image : Graphics.color array array;            (* Matrice prête à être convertie dans le format image de Graphics *) (* !!! 1ère coordonnée = ligne, 2nde = colonne *)
  mutable max_width : int;                               (*     Dimensions maximales du tableau image: par exemple lorsqu'on agrandit,*) 
  mutable max_height : int;                              (*     on alloue directement un tableau de taille max*)
 };;


(* abstraction du type image *)

(* change la couleur d'un pixel *)
let set_data img col row pix=   
  Matrix.set img.mod_data col row pix
;;

(* renvoit la couleur d'un pixel *)
let get_data img col row=        
  Matrix.get img.mod_data col row
;;

(* change l'intensité d'un pixel *)
let set_intensity img col row i= 
  Matrix.set img.intensity col row i
;;

(* renvoit l'intensité d'un pixel *)
let get_intensity img col row=   
  Matrix.get img.intensity col row
;;

(* change l'énergie d'un pixel (la première valeur du triplet, cf Types.energie) *)
let set_energy img col row e=    
  let(energy,bonus_agrandissement,bonus_protect)=Matrix.get img.energy col row in 
  Matrix.set img.energy col row (e,bonus_agrandissement,bonus_protect)
;;

(* renvoit l'énergie d'un pixel (la première valeur du triplet, cf Types.energie *)
let get_energy img col row=     
  let(energy,bonus_agrandissement,bonus_protect)=Matrix.get img.energy col row in 
    Matrix.set img.energy col row (energy,bonus_agrandissement*.4.0/.5.0,bonus_protect);  (* au passage, atténue le malus dû à l'agrandissement de l'image *)
    energy+.bonus_agrandissement+.bonus_protect;    
;;

(* modifie le bonus pour la protection ou la suppression d'une partie de l'image (3ème valeur du triplet) *)
let set_bonus_energy img col row b= 
  let(energy,bonus_agrandissement,ancien_bonus)=Matrix.get img.energy col row in   (* renvoit vrai si le bonus a été modifié, *)
    if ancien_bonus=b then                                                         (*pour éviter les recalculs (voir la fonction set_bonus) *)
      false 
    else
      begin
	Matrix.set img.energy col row (energy,bonus_agrandissement,b);
	true
      end
;;

(* efface les bonus (2ème et 3ème valeur du triplet énergie *)
let set_bonus_null img col row= 
  let(energy,bonus_agrandissement,ancien_bonus)=Matrix.get img.energy col row in 
    Matrix.set img.energy col row (energy,0.0,0.0)
;;

(* change l'énergie d'un chemin *)
let set_path_energy img col row pix= 
  Matrix.set img.path_energy col row pix
;;

(* renvoit l'énergie d'un chemin *)
let get_path_energy img col row=     
  Matrix.get img.path_energy col row
;;

(* pas très abstrait, mais c'est pour optimiser *)
let get_matrix_energy img=
  img.energy
;;
let get_matrix_path_energy img=
  img.path_energy
;;
let set_matrix_path_energy img m=
  img.path_energy<-m
;;

(* Pour connaître les dimensions de l'image *)
let width img = img.mod_width;;
let height img = img.mod_height;;
let max_width img = img.max_width;;
let max_height img = img.max_height;;

(* prépare l'affichage en créant une matrice des couleurs de type Graphics.color *)
let make_image pix mw mh w h=
  let t = Array.make_matrix mh mw (Graphics.black) in
  for j=0 to h-1 do
    for i = 0 to w-1 do
      t.(mh-1-j).(i) <- (Pixel.color_of_pixel (Matrix.get pix i j)) (* passe au système de couleur de Graphics *)
    done
    done;
      t
;;

(* Crée un objet image de type image *)
let make is_color maxw maxh w h pix_data=
  {
    color = is_color;
    width = w; 
    height = h;
    data = pix_data; 
    mod_width = w; 
    mod_height = h; 
    mod_data = pix_data; 
    intensity =  Matrix.make w h 0.0; 
    energy = Matrix.make w h Types.energy_null; 
    path_energy = Matrix.make w h Types.path_energy_null; 
    image = make_image pix_data maxw maxh w h;
    max_width=maxw;
    max_height=maxh;
  }
;;

(* Initialise les dimensions maximum qu'aura l'image: prépare le redimensionnement *)
let set_max_dims img w h=
  let mw= max img.mod_width w
  and mh= max img.mod_height h
  in
    img.max_width<-mw;
    img.max_height<-mh;
    img.image<-make_image img.mod_data mw mh (img.mod_width) (img.height)  (* initialise l'image: prêt à afficher *)
;;

(* sauve en pgm ou en ppm selon la source *)
let save im filename = 
  let w = width im and h = height im in
   let image_oc = open_out filename in
       if im.color then
	 begin
	   Printf.fprintf image_oc "P6\n%d %d\n255\n" w h;
	   print_string "L'image a été sauvée au format ppm.\n";
	 end	     
       else
	 begin
	   Printf.fprintf image_oc "P5\n%d %d\n255\n" w h;
	   print_string "L'image a été sauvée au format pgm.\n";
	 end;
     for j=h-1 downto 0 do   
       for i=0 to w-1 do 
	 Pixel.out image_oc (get_data im i j)
       done
     done;
  close_out image_oc 
;;

(* charge les pgm, les ppm et les bmp *)
let load filename= 
  let is_white_space c= (* reconnaît les caractères séparateurs pour les ppm, pgm *)
    match c with
      | '\n' | ' ' | '\t' | '\r' -> true
      | _ -> false
  in
  let read_number in_ch =
    let c = ref (input_char in_ch) in
    let s = ref "" in
      while not (is_white_space !c) do
	s:= !s ^ (Char.escaped !c);
	c:= input_char in_ch
      done;
      int_of_string !s
  in
  let read_header_ppm in_ch =  (* lit l'en-tête d'un ppm ou d'un pgm *)
    if is_white_space(input_char in_ch) then
	let width = read_number in_ch in
	let height= read_number in_ch in
	  if (read_number in_ch)= 255 then 
	    (width,height)
	  else
	    begin
	      close_in in_ch;
	      failwith("Erreur: les couleurs ne sont pas codées sur un octet.\n")
	    end
    else
      begin
	close_in in_ch;
	failwith("Ficher image corrompu.\n")
      end
  in
  let in_ch = open_in_bin filename in
    match input_char in_ch with
      | 'P' -> (* format ppm ou pgm *)
	  begin
	    match input_char in_ch with 
	      | '5' -> (* format pgm *)
		  begin
		    let (w,h) = read_header_ppm in_ch in
		    let pix_data = Matrix.make w h (Pixel.make_grey 0) in (* crée la matrice des pixels *)
		      (* lit les pixels *)
		      for j = 0 to h-1 do
			for i = 0 to w-1 do
			  Matrix.set pix_data i (h-1-j) (Pixel.make_grey (input_byte in_ch))
			done
			done;
			  close_in in_ch;
			  make false w h w h pix_data  (*création de l'image *)
		  end 
	      | '6' -> (* format ppm *)
		  begin
		    let (w,h) = read_header_ppm in_ch in
		    let pix_data = Matrix.make w h (Pixel.make_color 0 0 0) in (* crée la matrice des pixels *)
		      (* lit les pixels *)
		      for j = 0 to h-1 do
			for i = 0 to w-1 do
			  let r = input_byte in_ch in
			  let g = input_byte in_ch in
			  let b = input_byte in_ch in
			    Matrix.set pix_data i (h-1-j) (Pixel.make_color r g b) 
			done
			done;
			    close_in in_ch;		
			    make true w h w h pix_data (*création de l'image *)
		  end
	      | _ -> 
		  begin
		    close_in in_ch;
		    failwith("Format d'image non pris en charge.\n")
		  end
	  end
      | 'B' -> (* format bmp: cf les explications sur internet: http://www.commentcamarche.net/video/format-bmp.php3 *)
	  begin
	    let l = input_char in_ch in
	      if l = 'M' || l='A' then  (* commence par lire l'en-tête *)
		let _ =input_binary_int in_ch in
		let _ = input_binary_int in_ch in
		let _ = input_binary_int in_ch in
		let _ = input_binary_int in_ch in
		let w1 = input_byte in_ch in
		let w2 = input_byte in_ch in
		let w3 = input_byte in_ch in
		let w4 = input_byte in_ch in
		let w = (((w4 *256 + w3)*256 + w2)*256 + w1) in (* largeur *)
		let h1 = input_byte in_ch in
		let h2 = input_byte in_ch in
		let h3 = input_byte in_ch in
		let h4 = input_byte in_ch in
		let h = (((h4 *256 + h3)*256 + h2)*256 + h1) in (* hauteur *)
		let _ = input_byte in_ch in
		let _ = input_byte in_ch in
		let ncolor = input_byte in_ch in
		let _ = input_byte in_ch in
		  if ncolor = 24 then
		    begin
		      if input_binary_int in_ch = 0 then
  			let _ = input_binary_int in_ch in
			let _ = input_binary_int in_ch in
			let _ = input_binary_int in_ch in
			let nc_palette = input_binary_int in_ch in
			let _ = input_binary_int in_ch in
			  if nc_palette = 0 then                 (* fin de l'en-tête *)
			    begin
			      let pix_data = Matrix.make w h (Pixel.make_color 0 0 0) in (* crée la matrice des pixels *)
			      let reste = (4 - (3*w mod 4)) mod 4 in (* chaque ligne doit comporter n octects, avec n un multiple de 4, 
									reste est le nombre d'octets inutiles *)
				(* lit les pixels *)
				for j = 0 to h-1 do
				  for i = 0 to w-1 do
				    let b = input_byte in_ch in
				    let g = input_byte in_ch in
				    let r = input_byte in_ch in 
				      Matrix.set pix_data i j (Pixel.make_color r g b)
				  done;
				  (* lit les octects inutiles *)
				  for k = 1 to reste do
				    let _ = input_byte in_ch in ()
				  done;
				done;				    
				close_in in_ch;				    
			    	make true w h w h pix_data (*création de l'image *)
			    end
			  else
			    begin
			      close_in in_ch;
			      failwith("Palette des bitmaps non prise en charge.\n")
			    end
		      else
			begin
			  close_in in_ch;
			  failwith("Compression des bitmaps non pris en charge.\n")
			end
		    end
		  else
		    begin
		      close_in in_ch;
		      failwith("Format de couleur non pris en charge.\n")
		    end
	      else
		begin
		  close_in in_ch;
		  failwith("Format d'image non pris en charge.\n")
		end
	  end
      | l ->
	  begin
	    close_in in_ch;
	    failwith("Format d'image non pris en charge.\n")
	  end
;;

(* sauve en mémoire les changements appliqués à l'image *)
let save_modifications img=
  let w=img.mod_width
  and h=img.mod_height 
  in
    img.width<-w;
    img.height<-h;
    img.data<- Matrix.make w h (Matrix.get img.mod_data 0 0);
  for i=0 to w-1 do
    for j=0 to h-1 do
      Matrix.set img.data i j (Matrix.get img.mod_data i j)
    done
  done
;;

(* annule les changements appliqués à l'image *)
let cancel_modifications img=
  let w=img.width
  and h=img.height 
  in
    (* on restore tous les champs de image *)
    img.mod_width<-w;
    img.mod_height<-h; 
    img.max_width<-w;
    img.max_height<-h;
    img.mod_data<-Matrix.make w h (Matrix.get img.data 0 0);
    img.intensity<-Matrix.make w h 0.0; 
    img.energy<-Matrix.make w h Types.energy_null; 
    img.path_energy<-Matrix.make w h Types.path_energy_null; 
    img.image<-make_image img.data w h w h;
  for i=0 to w-1 do
    for j=0 to h-1 do
      Matrix.set img.mod_data i j (Matrix.get img.data i j)
    done
  done
;;


(* affiche l'image *)
let draw img x0 y0 =
  Graphics.draw_image (Graphics.make_image img.image) x0 y0;
;;

(* supprime un chemin du tableau image *)
let del_path_img img p=  (* !!! la première coordonnée du tableau doit être les lignes !!! *)
  let t = img.image in
  let maxheight = Array.length t-1 
  in
    match p with
      | Types.VPath(deb,dir) ->	(*chemin vertical*)
	  let w=width img in
	  let j=ref 0 and i0 = ref deb in
	  let del_fun _ = (* décale les pixels de la ligne !j *)
	    for i= !i0 to w-2 do
	      t.(maxheight- !j).(i)<-t.(maxheight- !j).(i+1)
	    done;
	    t.(maxheight- !j).(w-1)<- Graphics.black (* dernier pixel: en noir *)
	  in
	    del_fun (); (* on itère la fonction de suppression en mettant à jour i0 et j *)
	    List.iter (fun d ->
			 begin
			   match d with
			     | Types.L -> i0:= !i0-1; incr j
			     | Types.F ->             incr j
			     | Types.R -> i0:= !i0+1; incr j
			 end;
			 del_fun ()			     
		      ) dir
	      
      | Types.HPath (deb,dir)->	  (* idem que VPath *)
	  let h=height img in
	  let i=ref 0 and j0 = ref deb in
	  let del_fun _ = 
	    for j= !j0 to h-2 do
	      t.(maxheight-j).(!i)<-t.(maxheight-j-1).(!i)
	    done;
	    t.(maxheight-h+1).(!i)<-Graphics.black
	  in
	    del_fun ();
	    List.iter (fun d ->
			 begin
			   match d with
			     | Types.L -> j0:= !j0+1; incr i
			     | Types.F ->             incr i
			     | Types.R -> j0:= !j0-1; incr i
			 end;
			 del_fun ()			     
		      ) dir
;;

(* supprime un chemin de tous les tableaux et matrices de image *)
let rec del_path img p=
  Matrix.del_path (img.mod_data) p;
  Matrix.del_path (img.intensity) p;
  Matrix.del_path (img.energy) p;
  Matrix.del_path (img.path_energy) p;
  del_path_img img p;
  match p with  (* et on change les dimensions *)
    | Types.HPath _ -> img.mod_height <- img.mod_height-1 
    | Types.VPath _ -> img.mod_width <- img.mod_width-1
;;

(* ajoute un chemin au tableau image *)
let add_path_img img p = 
  let t = img.image in
  let maxheight = Array.length t-1 
  in
    match p with
      | Types.VPath(deb,dir) ->	
	  let w=width img in
	  let j=ref 0 and i0 = ref deb in
	  let del_fun _ = (* on décale tous les pixels d'une ligne donnée *)
	    for i= w downto !i0+1 do
	      t.(maxheight- !j).(i)<-t.(maxheight- !j).(i-1)
	    done;
	  in
	    del_fun (); (* on itère sur le chemin *)
	    List.iter (fun d ->
			 begin
			   match d with
			     | Types.L -> i0:= !i0-1; incr j
			     | Types.F ->             incr j
			     | Types.R -> i0:= !i0+1; incr j
			 end;
			 del_fun ()			     
		      ) dir
	      
      | Types.HPath (deb,dir)->	  (* idem que VPath *)
	  let h=height img in
	  let i=ref 0 and j0 = ref deb in
	  let del_fun _ = 
	    for j=h downto !j0+1 do
	      t.(maxheight-j).(!i)<-t.(maxheight-j+1).(!i)
	    done;
	  in
	    del_fun ();
	    List.iter (fun d ->
			 begin
			   match d with
			     | Types.L -> j0:= !j0+1; incr i
			     | Types.F ->             incr i
			     | Types.R -> j0:= !j0-1; incr i
			 end;
			 del_fun ()			     
		      ) dir
;;

(* Quand on ajoute un chemin:
   calcule la couleur du pixel ajouté en faisant la moyenne de celle de ses voisins
   initialise son intensité
   ajoute un malus d'énergie pour ne pas qu'il soit choisit la prochaine fois
*)
let update_new_path img p=
  let maxheight = Array.length img.image-1  
  in
    match p with
      | Types.VPath(deb,seq) ->
	  begin
	    let w=width img in 
	    let x=ref deb
	    and y=ref 0
	      (* 3 curseurs pour calculer la nouvelle couleur *)
	    and c_m = ref (Matrix.move_cursor img.mod_data deb 0; Matrix.get_cursor img.mod_data) in
	    let c_l = ref (if deb=0 then !c_m else Matrix.get_left !c_m) 
	    and c_r = ref (Matrix.get_right !c_m) (*pas de pb de bord à droite car on a ajouté un chemin *)
	      (* un curseur pour l'instensité *)
	    and c_intens = ref (Matrix.move_cursor img.intensity deb 0; Matrix.get_cursor img.intensity)
	      (* deux curseurs pour pénaliser les chemins de plus basse énergie *)
	    and c_en_m = ref (Matrix.move_cursor img.energy deb 0; Matrix.get_cursor img.energy) in
	    let c_en_r = ref (Matrix.get_right !c_en_m) (*pas de pb de bord à droite car on a ajouté un chemin *)
	    in
	    let move_cursor c d decalage =  (*déplace le curseur c dans la direction d *)
	      match d with
		| Types.L -> if 0<(!x+decalage) then c:=Matrix.get_left (Matrix.get_up !c)
		| Types.F -> c:=Matrix.get_up !c
		| Types.R -> if (!x+decalage)<w-1 then c:=Matrix.get_right (Matrix.get_up !c)
	    in
	    let interp () = (* mise à jour de tous les curseurs *)
	      let pix = Pixel.interpolation (Matrix.get_val !c_l) (Matrix.get_val !c_r) in
		Matrix.set_val !c_m pix;
		Matrix.set_val !c_intens (Pixel.intensity pix);
		img.image.(maxheight- !y).(!x) <- Pixel.color_of_pixel pix;
		let (e,bonus_path,bonus_protect) = Matrix.get_val !c_en_r
		in 
		  Matrix.set_val !c_en_r (e,bonus_path+.Types.malus_path,bonus_protect);
		  Matrix.set_val !c_en_m (e,bonus_path+.Types.malus_path,bonus_protect);
	    in
	      interp (); (* et on itère *)
	      List.iter (fun d->
			   move_cursor c_m d 0;
			   move_cursor c_l d (-1);
			   move_cursor c_r d 1;
			   move_cursor c_intens d 0;
			   move_cursor c_en_m d 0;
			   move_cursor c_en_r d 1;
			   begin
			     match d with
			       | Types.L -> x:= !x-1; incr y
			       | Types.F ->           incr y
			       | Types.R -> x:= !x+1; incr y
			   end;
			   interp()
			) seq		
	  end
      | Types.HPath(deb,seq) -> (* idem que pour VPath *)
	  begin
	    let h=height img in 
	    let x=ref 0
	    and y=ref deb
	    and c_m = ref (Matrix.move_cursor img.mod_data 0 deb; Matrix.get_cursor img.mod_data) in
	    let c_l = ref (Matrix.get_up !c_m)  (*pas de pb de bord en haut car on a ajouté un chemin *)
	    and c_r = ref (if deb=0 then !c_m else Matrix.get_down !c_m)
	    and c_intens = ref (Matrix.move_cursor img.intensity 0 deb; Matrix.get_cursor img.intensity) 
	    and c_en_m = ref (Matrix.move_cursor img.energy 0 deb; Matrix.get_cursor img.energy) in
	    let c_en_l = ref (Matrix.get_up !c_en_m) (*pas de pb de bord en haut car on a ajouté un chemin *)
	    in
	    let move_cursor c d decalage =  (*déplace le curseur c dans la direction d *)
	      match d with
		| Types.L -> if (!y+decalage)<h-1 then c:=Matrix.get_up (Matrix.get_right !c)
		| Types.F -> c:=Matrix.get_right !c
		| Types.R -> if 0<(!y+decalage) then c:=Matrix.get_down (Matrix.get_right !c)
	    in
	    let interp () =
	      let pix = Pixel.interpolation (Matrix.get_val !c_l) (Matrix.get_val !c_r) in
		Matrix.set_val !c_m pix;
		Matrix.set_val !c_intens (Pixel.intensity pix);
		img.image.(maxheight- !y).(!x) <- Pixel.color_of_pixel pix;
		let (e,bonus_path,bonus_protect) = Matrix.get_val !c_en_l
		in 
		  Matrix.set_val !c_en_l (e,bonus_path+.Types.malus_path,bonus_protect);
		  Matrix.set_val !c_en_m (e,bonus_path+.Types.malus_path,bonus_protect);
	    in
	      interp ();
	      List.iter (fun d->
			   move_cursor c_m d 0;
			   move_cursor c_l d 1;
			   move_cursor c_r d (-1);
			   move_cursor c_intens d 0;
			   move_cursor c_en_m d 0;
			   move_cursor c_en_l d 1;
			   begin
			     match d with
			       | Types.L -> y:= !y+1; incr x
			       | Types.F ->           incr x
			       | Types.R -> y:= !y-1; incr x
			   end;
			   interp()
			) seq		
	  end
;;

(* ajoute un chemin aux tableaux et matrices de l'image *)
let rec add_path img p=
  Matrix.add_path (img.mod_data) p;
  Matrix.add_path (img.intensity) p;
  Matrix.add_path (img.energy) p;
  Matrix.add_path (img.path_energy) p;
  add_path_img img p;
  begin
    match p with
      | Types.HPath _ -> img.mod_height <- img.mod_height+1 
      | Types.VPath _ -> img.mod_width <- img.mod_width+1
  end;
  update_new_path img p;
;;

 (* modifie l'énergie dans un cercle de rayon r, de centre (i,j) et colorie l'image en vert ou rouge *)
let set_bonus img i j r bonus= 
  let mh = img.max_height-1 in
  for x=i-r to i+r do
    for y=j-r to j+r do
      let dx = x-i 
      and dy = y-j 
      in
	if 0<=x && 0<=y && x<(width img) && y<(height img) then
	  if sqrt(float_of_int(dx*dx + dy*dy))<=float_of_int r then  (* si on est dans le cercle alor: *)
	      if set_bonus_energy img x y bonus then                 (* si on n'avais pas déjà le bon bonus, on le met et *)
		begin
		  img.image.(mh-y).(x)<- (                           (* on colorie en vert ou rouge *)
		    let (r,g,b) = Pixel.rgb_of_pixel (Matrix.get img.mod_data x y)
		    in
		      if bonus>0.0 then		   
			let ng = (g + 255)/2 in (* en vert *)
			  Graphics.rgb r ng b		   
		      else
			let nr = (r + 255)/2 in (* en rouge *)
			  Graphics.rgb nr g b
		  );
		end	
    done
  done
;;

(* remet les bonus d'energie à zéro *)
let erase_bonus img=
  let w=width img and h=height img in
    for i=0 to w-1 do
      for j=0 to h-1 do
	set_bonus_null img i j
      done
    done
;;
