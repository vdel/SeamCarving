(* représentation des "matrices" image: réseau de pixels défini par une étiquette et quatre voisins *)
type 'a pix_content = {
			mutable value: 'a;
			mutable up : 'a pix;
			mutable down: 'a pix;
			mutable left: 'a pix;
			mutable right: 'a pix
}    
and 'a pix = S | M of 'a pix_content ;;  (* side\middle *)

(* réseau de pixel: un pixel initial et sa position *)
type 'a matrix = {
		   mutable cursor: 'a pix;
                   mutable x_curs : int;
                   mutable y_curs : int
};;

(* nouveau pixel : tous ses voisins sont vides *)
let new_pix v= M {value = v; up=S ;  down=S ; left=S ; right=S } ;;

(* fonctions qui donnent le pixel à gauche, droite, haut, bas *)
let get_left p = 
  match p with
    |M content -> content.left
    |S -> failwith "pixel vide, left" 
;;

let get_right p = 
  match p with
    |M content -> content.right
    |S -> failwith "pixel vide, right" 
;;

let get_up p = 
  match p with
    |M content -> content.up
    |S -> failwith "pixel vide, up" 
;;

let get_down p = 
  match p with
    |M content -> content.down
    |S -> failwith "pixel vide, down" 
;;

(* la valeur du pixel *)
let get_val p = 
  match p with
    |M content -> content.value
    |S -> failwith "get: pixel vide" 
;;

let get_cursor m = m.cursor;;

(* la série des set : pour changer les pointeurs vers les voisins *)
let set_left p pbis = 
  match p with
    |M content -> content.left <- pbis
    |S -> ()
;;

let set_right p pbis =
  match p with
    |M content -> content.right <- pbis
    |S -> ()
;;

let set_up p pbis = 
  match p with
    |M content -> content.up <- pbis
    |S -> ()
;;

let set_down p pbis = 
  match p with
    |M content -> content.down <- pbis
    |S -> ()
;;

let set_val p v=
  match p with
    |M content -> content.value <- v
    |S -> ()
;;

(* déplace le curseur de m aux coordonées (i,j) *)
let move_cursor (m : 'a matrix) i j=
  if m.x_curs<>i then
    begin
      if i > m.x_curs then (* si pixel demandé à droite du curseur... *)
	for k=m.x_curs to i-1 do
	  m.cursor <- get_right m.cursor
	done
      else for k=i to m.x_curs-1 do (* sinon à gauche *)
          m.cursor <- get_left m.cursor
      done;
      m.x_curs<-i
    end;
  if m.y_curs<>j then
    begin
      if j > m.y_curs then (* si le pixel demandé est au-dessus du curseur... *)
	for k=m.y_curs to j-1 do
	  m.cursor <- get_up m.cursor
	done
      else for k=j to m.y_curs-1 do  (* sinon au-dessous *)
        m.cursor <- get_down m.cursor
      done;
      m.y_curs<-j
    end
;;

(* contenu du pixel en place (i,j) dans l'image *)
let get (m: 'a matrix) i j =
  move_cursor m i j;
  get_val m.cursor
;;

(* la même en set *)
let set (m: 'a matrix) i j v = 
  move_cursor m i j;
  set_val m.cursor v
;;

(* pour créer une nouvelle matrice : (0,0) est en bas à gauche *)
let make w h e = { cursor = begin
		       let x0 = new_pix e in
		       let curs_down = ref S in (* pixel vide *)
			 for j = 0 to h-1 do (* faire ajouter un pixel au dessus de curs_down avec trois pixels vides commes autres voisins*)
			   begin
			     let curs_left = ref (
						   match !curs_down with 
						     | S -> x0  (* au-début, curs_left, c'est x0 *)
						     | M pix -> let new_pixel = M {value = e; up = S; down = !curs_down; left = S; right = S} in  (* après, on crée un nouveau pixel *)
								  begin
								    pix.up<-new_pixel;  (* au dessus, de curs_down et on fait le lien entre les deux *)
								    curs_down:= get_right !curs_down;  (* on déplace curs_down pour qu'il soit en dessous du *)
								    new_pixel                          (* prochain pixel créé *)
								  end
						 )
			     in 
			     let next_curs_down = !curs_left in 
			       for i=1 to w-1 do (* rajoute un pixel à droite de curs_left, complète les lignes *)
				 let new_pixel = M {value = e; up = S; down = !curs_down; left= !curs_left; right = S} in
				   set_right !curs_left new_pixel;    (* on fait les lien entre voisins *)
				   if j<>0 then
				     begin
				       set_up !curs_down new_pixel;  (* toujours les voisins *)		       
				       curs_down := get_right !curs_down  (* on déplace encore curs_down d'un cran à droite *) 
				     end;
				   curs_left := new_pixel;  (* on déplace curs_left à droite *)
			       done;
			       curs_down := next_curs_down;  (* on remet curs_down à jour (ancien curs_left) *)
			   end
			 done;
			 x0
		   end;    
		   x_curs = 0;
		   y_curs = 0;
		 }
;;

(* pour faire une matrice quand on connait les coordonnées du curseur que l'on manipule *)
let explicit_make c i j= {cursor=c; x_curs=i; y_curs=j};;

(* pour créer un tableau à partir d'une matrice *)
let array_of_matrix m =
  move_cursor m 0 0; (* coin en bas à gauche *)
  let o=ref m.cursor in
  let w= ref 0 and h = ref 0
  in
    begin (* taille de la matrice dans chaque direction *)
      try
	while true do
	  o:=get_right !o;
	  incr w  
	done
      with 
	| _ ->
	    begin
	      try
		o:=m.cursor;
		while true do
		  o:=get_up !o;
		  incr h
		done
	      with 
	      | _ -> ()
	    end
    end;
    let c=ref m.cursor in
    let t= Array.make_matrix !w !h (get_val !c) in
      o:= !c;
      for i=0 to !w-1 do
	if i<>(!w-1) then
	  o:=get_right !o; (* !o est le curseur de bas de colonne *)
	for j=0 to !h-1 do
	  t.(i).(j)<-get_val !c;  (* et on copie *)
	  if j<>(!h-1) then  
	    c:=get_up !c  (* !c parcourt toute la colonne *)
	done;
	c:= !o
      done;
      t
;;

(* Utilisé dans Path.update_path, ne recopie pas toute la matrice mais,seulement après l'endroit ou des pixels peuvent être modifiés ie deux avant le chemin *)
let special_array_of_matrix m p=
  match p with
    | Types.VPath(deb,_) ->
	begin (* toujours taille dans les deux directions *)
	  move_cursor m 0 0;
	  let o=ref m.cursor in
	  let w= ref 0 and h = ref 0
	  in
	    begin
	      try
		while true do
		  o:=get_right !o;
		  incr w  
		done
	      with 
		| _ ->
		    begin
		      try
			o:=m.cursor;
			while true do
			  o:=get_up !o;
			  incr h
			done
		      with 
			| _ -> ()
		    end
	    end;
	    let d=ref deb and f=ref deb in  (* les pixels à copier sont compris sur une même ligne entre d et f *)
	    let move_d () = if 0< !d then d:= !d-1 else d:=0 (* une ligne après, d<-d-1 sauf au bord *)
	    and move_f () = if !f< !w-1 then f:= !f+1 else f:= !w-1  (* de même, f<-f+1 sauf au bord *)
	    in
	      (* on est en deb 0, on doit copie de (deb-2, 0) à (deb+2, 0) (faut faire un dessin en reprenant l'algo de calcul du poids des chemins) *)
	      move_d ();
	      move_d ();
	      move_f ();
	      move_f ();
	      move_cursor m !d 0;    
	      o:=m.cursor;
	      let t= Array.make_matrix !w !h (get_val !o)
	      and c= ref !o in
		for j=0 to !h-1 do  (* on copie joyeusement *)
		  c:= !o;
		  t.( !d).(j)<-get_val !o; (* !c: curseur représentant l'extrémité gauche de la ligne, !o parcourt toute la ligne *)		  
		  for i= !d+1 to !f do
		    o:= get_right !o;
		    t.(i).(j)<-get_val !o; 
		  done;
		  if !d=0 then   (* on met à jour !o (en fait !c, voir début de boucle) : on prend le pixel au-dessus à gauche *)
		    o:=get_up !c
		  else
		    begin
		      o:=get_left !c;
		      o:=get_up !o
		    end;
		  move_d ();
		  move_f ();
		done;
		t
	end
    | Types.HPath(deb,_) -> (* idem que VPath *)
	begin
	  move_cursor m 0 0;
	  let o=ref m.cursor in
	  let w= ref 0 and h = ref 0
	  in
	    begin
	      try
		while true do
		  o:=get_right !o;
		  incr w  
		done
	      with 
		| _ ->
		    begin
		      try
			o:=m.cursor;
			while true do
			  o:=get_up !o;
			  incr h
			done
		      with 
			| _ -> ()
		    end
	    end;
	    let d=ref deb and f=ref deb in
	    let move_d () = if 0< !d then d:= !d-1 else d:=0
	    and move_f () = if !f< !h-1 then f:= !f+1 else f:= !h-1
	    in
	      move_d ();
	      move_d ();
	      move_f ();
	      move_f ();
	      move_cursor m 0 !d;    
	      o:=m.cursor;
	      let t= Array.make_matrix !w !h (get_val !o)	
	      and c= ref !o in
		for i=0 to !w-1 do
		  c:= !o;
		  t.(i).(!d)<-get_val !o; 		  
		  for j= !d+1 to !f do
		    o:= get_up !o;
		    t.(i).(j)<-get_val !o;
		  done;
		  if !d=0 then 
		    o:=get_right !c
		  else
		    begin		      
		      o:=get_down !c;		      
		      o:=get_right !o;
		    end;
		  move_d ();
		  move_f ();
		done;
		t;
	end
;;

(* copie le tableau t dans m *)
let copy_array t m w h=
  move_cursor m 0 0;
  let o=ref m.cursor in
  let c=ref m.cursor in
    for i=0 to w-1 do  (* on copie *)
      if i<>(w-1) then
	o:=get_right !o;  (* o: bas de colonne *)
      for j=0 to h-1 do	
	set_val !c (t.(i).(j));
	if j<>(h-1) then
	  c:=get_up !c   (* c parcourt toute la colonne *)
      done;
      c:= !o
    done
;;

(* Utilisé dans Path.update_path, ne recopie pas toute la matrice, idem que special_array_of_matrix *)
let special_copy_array t m p w h=
  match p with
    | Types.VPath(deb,_) ->
	begin
	  let d=ref deb and f=ref deb in
	  let move_d () = if 0< !d then d:= !d-1 else d:=0
	  and move_f () = if !f<w-1 then f:= !f+1 else f:=w-1
	  in
	    move_d ();
	    move_d ();
	    move_f ();
	    move_f ();
	    move_cursor m !d 0;
	    let o= ref m.cursor in
	    let c= ref !o in
	      for j=0 to h-1 do
		c:= !o;
		set_val !o t.( !d).(j); 		  
		for i= !d+1 to !f do
		  o:= get_right !o;
		  set_val !o t.(i).(j); 
		done;
		if !d=0 then 
		  o:=get_up !c
		else
		  begin
		    o:=get_left !c;
		    o:=get_up !o
		  end;
		move_d ();
		move_f ();
	      done
	end
    | Types.HPath(deb,_) ->
	begin
	  let d=ref deb and f=ref deb in
	  let move_d () = if 0< !d then d:= !d-1 else d:=0
	  and move_f () = if !f<h-1 then f:= !f+1 else f:=h-1
	  in
	    move_d ();
	    move_d ();
	    move_f ();
	    move_f ();
	    move_cursor m 0 !d;
	    let o= ref m.cursor in
	    let c= ref !o in
	      for i=0 to w-1 do
		c:= !o;
		set_val !o t.(i).(!d); 		  
		for j= !d+1 to !f do
		  o:= get_up !o;
		  set_val !o t.(i).(j); 
		done;
		if !d=0 then 
		  o:=get_right !c
		else
		  begin
		    o:=get_down !c;
		    o:=get_right !o
		  end;
		move_d ();
		move_f ();
	      done;
	end
;;	      

(* supprime le chemin : on redonne à chaque fois les bons voisins. A chaque étape on remet en place les liaisons haut, droite et gauche de la ligne/colonne considérée, plus les liaisons basses de la ligne du dessus/colonne d'après *)
let del_path (m : 'a matrix) p =
  let del_H_F pxl = (* dans chemin horizontal, pixel au même niveau *)
    match pxl with
      |M content -> 
	 begin 
           set_up content.down content.up;
           set_down content.up content.down;          
         end
      |S -> ()  (* erreur? *)
  in
  let del_H_L pxl = (* dans chemin horizontal, pixel un cran dessus *)
    match pxl with
      |M content -> 
	 begin 
           set_up content.down content.up;
           set_down content.up content.down;
           set_right content.left content.down;
           set_left content.down content.left
         end
      |S -> ()
  in
  let del_H_R pxl = (* dans chemin horizontal, pixel un cran dessous *)
    match pxl with
      |M content ->
	 begin 
           set_up content.down content.up;
           set_down content.up content.down;
           set_right content.left content.up;
           set_left content.up content.left
         end
      |S -> ()
  in
  let del_V_F pxl = (* dans chemin vertical, pixel au même niveau *)
    match pxl with
      |M content -> 
	 begin 
           set_right content.left content.right;
           set_left content.right content.left;
         end
      |S -> ()
  in
  let del_V_L pxl =  (* dans chemin vertical, pixel un cran à gauche *)
    match pxl with
      |M content -> 
	 begin
           set_right content.left content.right;
           set_left content.right content.left;
           set_down content.right content.down;
           set_up content.down content.right 
         end
      |S -> ()
  in
  let del_V_R pxl = (* dans chemin vertical, pixel un cran à droite *)
    match pxl with
      |M content -> 
	 begin 
           set_right content.left content.right;
           set_left content.right content.left;
           set_down content.left content.down;
           set_up content.down content.left
	 end
      |S -> ()
  in 
    match p with
      | Types.HPath(deb,dir) -> let rpix = ref (move_cursor m 0 deb; m.cursor) in
				  begin
				    del_H_F (!rpix) ; (* le premier *)
				    List.iter (fun d ->  match d with
			                         | Types.L -> rpix:= get_right (get_up !rpix) ; del_H_L (!rpix)
			                         | Types.F -> rpix:= get_right !rpix ; del_H_F (!rpix)
			                         | Types.R -> rpix:= get_right (get_down !rpix) ; del_H_R (!rpix)	     
		                              ) dir; (* à chaque étape on descend et on enlève grace aux fonctions auxilliaires *)
				    if m.y_curs<>0 then (* le curseur se trouve là où passe le chemin, il faut le mettre à jour *)
				      begin
					m.cursor<-get_down m.cursor;
					m.y_curs<-m.y_curs-1
				      end
				    else
				      m.cursor<-get_up m.cursor;
				  end
      | Types.VPath (deb,dir) -> let rpix = ref (move_cursor m deb 0; m.cursor) in
				   begin
				     del_V_F (!rpix) ;
				     List.iter (fun d ->  match d with
			                          | Types.L -> rpix:= get_up (get_left !rpix) ; del_V_L (!rpix)
			                          | Types.F -> rpix:= get_up !rpix ; del_V_F (!rpix)
			                          | Types.R -> rpix:= get_up (get_right !rpix) ; del_V_R (!rpix)	     
		                               ) dir; (* à chaque étape on descend et on enlève grace aux fonctions auxilliaires *)
				     if m.x_curs<>0 then (* le curseur se trouve là où passe le chemin, il faut le mettre à jour *)
				       begin
					 m.cursor<-get_left m.cursor;
					 m.x_curs<-m.x_curs-1
				       end
				     else
				       m.cursor<-get_right m.cursor;
				   end ;;	              

(* cette fois on ajoute un chemin en dédoublant un chemin existant *) 	     
let add_path m p =
  match p with
      | Types.HPath(deb,dir) ->
	  begin
	    let act_pix = ref (move_cursor m 0 deb; m.cursor) 
	    and last_pix = ref S in
	    let c =get_val !act_pix in
	    let f direc = (* fonction qui insère un pixel dans la matrice *)
	      let np = new_pix c in (* pixel inséré *)
	      let d = get_down !act_pix in
		(* on fait les branchements verticaux *)
		set_up d np;
		set_down np d;
		set_up np !act_pix;
		set_down !act_pix np;
		(* puis les branchements horizontaux *)
		begin
		  match direc with
		    | Types.L -> 
			begin
			  let l=get_left d in
			    set_right l np;
			    set_left np l;
			    set_right !last_pix d;
			    set_left d !last_pix;
			end
		    | Types.F ->
			begin
			  set_right !last_pix np;
			  set_left np !last_pix;
			end
		    | Types.R -> 
			begin
			  let l=get_left !act_pix in
			    set_right l np;
			    set_left np l;
			    set_right !last_pix !act_pix;
			    set_left !act_pix !last_pix;
			end
		end;
		last_pix:= np;
	    in
	      f (Types.F);
	      List.iter (fun d->
			   (* on met à jour act_pix *)
			   begin	      
			     match d with
			       | Types.L -> act_pix:= get_right (get_up !act_pix)
			       | Types.F -> act_pix:= get_right !act_pix
			       | Types.R -> act_pix:= get_right (get_down !last_pix)
			   end;
			   (* et on lui applique f *)
			   f d
			) dir;
	      m.y_curs <- m.y_curs+1;
	  end
      | Types.VPath(deb,dir) -> (* pareil que HPath *)
	  begin
	    let act_pix = ref (move_cursor m deb 0; m.cursor) 
	    and last_pix = ref S in
	    let c =get_val !act_pix in	     
	    let f direc =  (* fonction qui insère un pixel dans la matrice *)
	      let np = new_pix c in (* pixel inséré *)
	      let l = get_left !act_pix in
		(* on fait les branchements horizontaux *)
		set_right l np;
		set_left np l;
		set_right np !act_pix;
		set_left !act_pix np;
		(* puis les branchements verticaux *)
		begin
		  match direc with
		    | Types.L -> 
			begin
			  let d=get_down !act_pix in
			    set_up d np;
			    set_down np d;
			    set_up !last_pix !act_pix;
			    set_down !act_pix !last_pix;
			end
		    | Types.F ->
			begin
			  set_up !last_pix np;
			  set_down np !last_pix;
			end
		    | Types.R -> 
			begin
			  let d=get_down l in
			    set_up d np;
			    set_down np d;
			    set_up !last_pix l;
			    set_down l !last_pix;
			end
		end;
		last_pix:= np;
	    in
	      f (Types.F); 
	      List.iter (fun d ->
			   (* on met à jour act_pix *)
			   begin	      
			     match d with
			       | Types.L -> act_pix:= get_up (get_left !last_pix)
			       | Types.F -> act_pix:= get_up !act_pix
			       | Types.R -> act_pix:= get_up (get_right !act_pix)
			   end;
			   f d
			) dir;      
	      m.x_curs <- m.x_curs+1;
	  end
;;

    
                             
