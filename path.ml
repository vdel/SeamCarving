(* initialise la matrice du poids des chemins dans une direction donnée *)
let calc direction img =
  let w = Image.width img and h = Image.height img in
  let m = Image.get_matrix_energy img in
    (* on convertit la matrice des énergies en tableau, ca permet d'accelérer le calcul *)
  let e = Matrix.array_of_matrix m in
  let energy i j = let (ener,bonus_path,bonus_protec) = e.(i).(j) in ener+.bonus_path+.bonus_protec in  (* l'énergie d'un pixel: somme de la fonction d'énergie et des bonus *)
  let paths = Array.make_matrix w h (energy 0 0) in
    begin
      match direction with
	| Types.V -> (* si on veut un chemin vertical *)
	    begin
	      (* par programmation dynamique: on initialise la ligne du bas avec les énergies *)
	      for i=0 to (w-1) do
		paths.(i).(0)<- energy i 0 
	      done;	      
	      (* par récurence paths.(i).(j) = energy i j + min ( paths.(i-1).(j-1) paths.(i).(j-1) paths.(i+1).(j-1) ) *)
	      for j=1 to (h-1) do
		(* attention aux bords *)
		paths.(0).(j)<- energy 0 j +. min (paths.(0).(j-1)) (paths.(1).(j-1));
		for i=1 to (w-2) do
		  paths.(i).(j)<- energy i j +. min (paths.(i-1).(j-1)) (min (paths.(i).(j-1)) (paths.(i+1).(j-1)))
		done;
		paths.(w-1).(j)<- energy (w-1) j +. min (paths.(w-1).(j-1)) (paths.(w-2).(j-1));
	      done
	    end
	| Types.H ->  (* idem *)
	    begin
	      for j=0 to (h-1) do
		paths.(0).(j)<- energy 0 j
	      done;
	      for i=1 to (w-1) do
		paths.(i).(0)<- energy i 0 +. min (paths.(i-1).(0)) (paths.(i-1).(1));
		for j=1 to (h-2) do
		  paths.(i).(j)<- energy i j +. min (paths.(i-1).(j-1)) (min (paths.(i-1).(j+1)) (paths.(i-1).(j)))
		done;
		paths.(i).(h-1)<- energy i (h-1) +. min (paths.(i-1).(h-1)) (paths.(i-1).(h-2));
	      done
	    end
    end;
    (* on repasse en matrice *)
    Matrix.copy_array paths (Image.get_matrix_path_energy img) w h
;;


let find direction img =
  let w = ((Image.width img)-1) in 
  let h = ((Image.height img) -1) in
    match direction with
      | Types.V ->  (* on veut un chemin vertical *)
	  begin 	 
	    (* on stocke dans pos l'abscisse du chemin de poids minimal *)
	    let pos = ref 0 in   (* on recherche le début du chemin en regardant la plus faible valeur dans la première colonne *)
	    let mini = ref (Image.get_path_energy img !pos h) in
	      for i= !pos+1 to w do
		let v = Image.get_path_energy img i h in
		  if v < !mini then
		    begin
		      pos :=i;
		      mini:=v
		    end
	      done;
	      (* on a le début du chemin: on trouve la suite en cherchant le minimum des voisins du dessous *)
	      let path_min = ref [] in (* le chemin minimal *)   
		for j=(h-1) downto 0 do
		  let v = !pos in
		    if (0<v)&&(v<w) then
		      begin
			(* les voisins du dessous *)
			let buff_l = Image.get_path_energy img (v+1) j in
			let buff_f = Image.get_path_energy img v j in
			let buff_r = Image.get_path_energy img (v-1) j in
			  (* on trouve le minimum *)
			  if  buff_f <= buff_l then
			    begin			    
			      if buff_f <= buff_r then 
				path_min := Types.F::(!path_min)
			      else 
				begin path_min := Types.R::(!path_min); 
				  pos:= (!pos-1) 
				end
			    end
			  else 
			    begin
			      if buff_l <= buff_r then
				begin 
				  path_min:= Types.L::(!path_min); 
				  pos:= (!pos+1) 
				end
			      else 
				begin
				  path_min := Types.R::(!path_min);
				  pos:=(!pos-1)
				end
			    end	    
		      end
		    else
		      begin
			(* attention aux bords ! *)
			if v=0 then 
			  begin
			    let buff_l = Image.get_path_energy img 1 j in
			    let buff_f = Image.get_path_energy img 0 j in
			      if buff_f <= buff_l then
				path_min := Types.F::(!path_min)
			      else 
				begin 
				path_min:= Types.L::(!path_min); 
				  pos:= (!pos+1) 
				end
			  end
			else
			  begin 
			    let buff_f = Image.get_path_energy img w j in
			    let buff_r = Image.get_path_energy img (w-1) j in
			      if buff_f <= buff_r then
				path_min := Types.F::(!path_min)
			      else  
				begin 
				  path_min := Types.R::(!path_min); 
				  pos:= (!pos-1) 
				end
			  end
		      end
		done;		
		Types.VPath (!pos, !path_min)
	  end  
      | Types.H ->  (* pareil que pour Types.V *)
	  begin 
	    let pos = ref 0 in   (* on recherche le début du chemin en regardant la plus faible valeur dans la première colonne *)	     
	    let mini = ref (Image.get_path_energy img w !pos) in
	      for j= !pos+1 to h do  
		let v = Image.get_path_energy img w j in
		  if v < !mini then
		    begin
		      pos :=j;
		      mini:=v
		    end
	      done;
	      let path_min = ref [] in (* le chemin minimal *)		
		for i=(w-1) downto 0 do
		  let v = !pos in
		    if (0<v)&&(v<h) then
		      begin
			let buff_r = Image.get_path_energy img i (v+1) in
			let buff_f = Image.get_path_energy img i v in
			let buff_l = Image.get_path_energy img i (v-1) in
			  if buff_f < buff_r then
			    begin
			      if buff_f <= buff_l then 
				path_min := Types.F::(!path_min)
			      else 
				begin 
				  path_min := Types.L::(!path_min); 
				  pos:= (!pos-1) 
				end
			    end
			  else 
			    if buff_r <= buff_l then
			      begin 
				path_min:= Types.R::(!path_min); 
				pos:= (!pos+1) 
			      end
			    else
			      begin 
				path_min := Types.L::(!path_min);
				pos:=(!pos-1)
			      end
		      end
		    else 
		      begin
			if v=0 then
			  begin
			    let buff_r = Image.get_path_energy img i 1 in
			    let buff_f = Image.get_path_energy img i 0 in
			      if buff_f <= buff_r then 
				path_min := Types.F::(!path_min)
			      else 
				begin 
				  path_min:= Types.R::(!path_min); 
				  pos:= (!pos+1) 
				end
			  end
			else 
			  begin
			    let buff_f = Image.get_path_energy img i h in
			    let buff_l = Image.get_path_energy img i (h-1) in
			      if buff_f <= buff_l then
				path_min := Types.F::(!path_min)
			      else 
				begin 
				  path_min := Types.L::(!path_min); 
				  pos:= (!pos-1) 
				end
			  end
		      end
		done;
		Types.HPath (!pos, !path_min)
	  end
;;

let rec update_line_V p e w h deb=
  let t1=Array.make w 0 in (* tableau des pixels à examiner, si un pixel doit être mis à jour, alors *)
  let t2=Array.make w 0 in (* ses trois voisins du dessus doivent être examinés *)
  let t=ref t1 
  and next_t= ref t2 
  and d = ref 0  (* bornes gauche et droite des pixels à examiner *)
  and f = ref 0
  and next_d = ref (deb-1)  (* prochaines valeurs de d et f *)
  and next_f = ref (deb+1)
  in
  let set t i j =   (* il faudra examiner le pixel (i,j) *)
    if 0<=i && i<w then t.(i)<-j
  in
  let change_t () =  (* on organise une rotation sur t1 et t2: pas besoin de les effacer *)
    if !t = t1 then (t:=t2; next_t:=t1) else (t:=t1; next_t:=t2)
  in
    (* les trois premiers pixel à devoir être examinés *)
    set !t (deb-1) 1;
    set !t deb 1;
    set !t (deb+1) 1;
    for j=1 to h-1 do
      (* on met d et f à jour *)
      if !next_d<0 then d:=0
      else d:= !next_d;
      if !next_f>=w then f:=w-1
      else f:= !next_f;
      next_d:=w-1;
      next_f:=0;
      (* on parcourt la ligne *)
      for i= !d to !f do
	if !t.(i) = j then 
	  if update_pix_V p e w h i j then (* si on a du modifier le pixel alors *)
	    begin
	      (* on examinera les trois voisins du dessus *)
	      set !next_t (i-1) (j+1);
	      set !next_t i (j+1);
	      set !next_t (i+1) (j+1);
	      (* éventuellement, on repousse les bornes pour la prochaine ligne *)
	      if (i-1)< !next_d then next_d:=i-1;
	      if (i+1)> !next_f then next_f:=i+1
	    end
      done;
      (* on effectue la rotation de t1 et t2 *)
      change_t ()
    done   
and update_pix_V p e w h i j =
  let a = try p.(i-1).(j-1) with _ -> Types.path_energy_inf
  and b = try p.(i).(j-1)   with _ -> Types.path_energy_inf
  and c = try p.(i+1).(j-1) with _ -> Types.path_energy_inf 
  in
    (* on calcule le minimum des voisins d'en-dessous + la valeur de l'énergie en i,j *)
  let mini = (min (min a b) c) +. (let (energy,bonus_path,bonus_protec)=e.(i).(j) in energy+.bonus_path+.bonus_protec) in
    (* s'il est différent de la valeur du chemin en (i,j) *)
    if mini<>p.(i).(j) then
      begin 
        p.(i).(j)<-mini;  (* on met à jour *)
	true;
      end
    else
      false
;;

let rec update_column_H p e w h deb=   (* pareil que update_line_V *)
  let t1=Array.make h 0 in
  let t2=Array.make h 0 in
  let t=ref t1 
  and next_t= ref t2 
  and d = ref 0
  and f = ref 0
  and next_d = ref (deb-1)
  and next_f = ref (deb+1)
  in
  let set t i j =
    if 0<=j && j<h then t.(j)<-i
  in
  let change_t () =
    if !t = t1 then (t:=t2; next_t:=t1) else (t:=t1; next_t:=t2)
  in
    set !t 1 (deb-1);
    set !t 1 deb;
    set !t 1 (deb+1);
    for i=1 to w-1 do
      if !next_d<0 then d:=0
      else d:= !next_d;
      if !next_f>=h then f:=h-1
      else f:= !next_f;
      next_d:=h-1;
      next_f:=0;
      for j= !d to !f do
	if !t.(j) = i then
	  if update_pix_H p e w h i j then
	    begin
	      set !next_t (i+1) (j-1);
	      set !next_t (i+1) j;
	      set !next_t (i+1) (j+1);
	      if (j-1)< !next_d then next_d:=j-1;
	      if (j+1)> !next_f then next_f:=j+1
	    end
      done;
      change_t ()
    done
and update_pix_H p e w h i j =
  let a = try p.(i-1).(j-1) with _ -> Types.path_energy_inf
  and b = try p.(i-1).(j)  with _ -> Types.path_energy_inf
  and c = try p.(i-1).(j+1) with _ -> Types.path_energy_inf 
  in
  let mini = (min (min a b) c) +. (let (energy,bonus_path,bonus_protec)=e.(i).(j) in energy+.bonus_path+.bonus_protec) in
    if mini<>p.(i).(j) then
      begin 
        p.(i).(j)<-mini;
	true;
      end
    else
      false
;;

(* les coordonnées des chemins suceptibles d'être modifiés forment
   une sorte de triangle partant du pixel de début du chemin modifié. *)
let update_path img p = 
  let paths=Matrix.special_array_of_matrix (Image.get_matrix_path_energy img) p in
  let energy=Matrix.special_array_of_matrix (Image.get_matrix_energy img) p in
  let w=Image.width img and h=Image.height img in
    begin
      match p with
	|Types.HPath(deb,_) -> update_column_H paths energy w h deb
	|Types.VPath(deb,_) -> update_line_V paths energy w h deb
    end;
    Matrix.special_copy_array paths (Image.get_matrix_path_energy img) p w h;
;;


(* Affiche la matrice des énergies des chemins, pour débugger *)
let draw_mat img x0 y0= 
  let w=Image.width img and h=Image.height img in
  let color_matrix = Array.make_matrix h w (Graphics.rgb 0 0 0) in
  let max_energy = ref (-10000.0) 
  and min_energy = ref (10000.0) in
    for j=0 to h-1 do
      for i = 0 to w-1 do
	let v = Image.get_path_energy img i j in 
	  if !max_energy < v then max_energy:= v;
	  if !min_energy > v then min_energy:= v
      done
    done;
    for j=0 to h-1 do
      for i = 0 to w-1 do
	color_matrix.(h-1-j).(i) <- 
	  let c = int_of_float(((Image.get_path_energy img i j-. !min_energy) /. (!max_energy-. !min_energy)) *. 255.0) in
	    Graphics.rgb c c c
      done
    done;
    Graphics.draw_image (Graphics.make_image color_matrix) x0 y0;
;;


(* Affiche le chemin *)
let draw img p x0 y0=
  let plot i j = Graphics.plot (x0+i) (y0+j)
  in
    Graphics.set_color (Graphics.red);
    match p with
      | Types.VPath(deb,seq) -> 
	  let x=ref deb and y=ref 0
	  in
	    begin
	      try
		plot !x !y;  (* itère plot en faisant varier !x et !y *)
		List.iter (fun d->
			     begin
			       match d with
				 | Types.L -> x:= !x-1; incr y
				 | Types.F ->           incr y
				 | Types.R -> x:= !x+1; incr y
			     end;
			     plot !x !y
			  ) seq		
	      with | _ -> ()	       
	    end
      | Types.HPath(deb,seq) -> 
	  let x=ref 0 and y=ref deb
	  in
	    begin
	      try
		plot !x !y;
		List.iter (fun d->
			     begin
			       match d with
				 | Types.L -> y:= !y+1; incr x
				 | Types.F ->           incr x
				 | Types.R -> y:= !y-1; incr x
			     end;
			     plot !x !y
			  ) seq
	      with | _ -> ()
	    end
;;

(* tentative ratée d'optimisation *)
(*let rec update_line_H img d=
  let w=Image.width img and h=Image.height img in
  let deb = min d (h-1) in  
  let t1=Array.make h 0 in
  let t2=Array.make h 0 in
  let t=ref t1 
  and next_t= ref t2 
  and d = ref 0
  and f = ref 0
  and next_d = ref (deb-1) 
  and next_f = ref (deb+1)
  and mat_e = Image.get_matrix_energy img 
  and mat_p = Image.get_matrix_path_energy img
  and paths = Array.make_matrix w h None in
    Matrix.move_cursor mat_e 0 deb;
    Matrix.move_cursor mat_p 0 deb; 
    let curs_e = ref (Matrix.get_cursor mat_e)
    and curs_p = ref (Matrix.get_cursor mat_p)
    in
    let set t i j =
      if 0<=j && j<h then t.(j)<-i
    in
    let change_t () =
      if !t = t1 then (t:=t2; next_t:=t1) else (t:=t1; next_t:=t2)
    in
    let eval_p i j=
      if i<0 || j<0 || i>=w || j>=h then failwith("Erreur Ã  Path.update")
      else
	match paths.(i).(j) with
	  | None -> let v = Image.get_path_energy img i j in paths.(i).(j)<-Some v; v
	  | Some v -> v
    in
    let update_cursors incr_i anc_e anc_p anc_i anc_j new_j=
      if incr_i then
	begin
	  for k=anc_j to new_j-1 do (* on va vers le haut *)
	    anc_e:= Matrix.get_up !anc_e;
	    anc_p:= Matrix.get_up !anc_p
	  done;	 
	  for k=new_j to anc_j-1 do (* on va vers le bas *)
	    anc_e:= Matrix.get_down !anc_e;
	    anc_p:= Matrix.get_down !anc_p
	  done;
	  if new_j=0 then
	    Image.set_matrix_path_energy img (Matrix.explicit_make !curs_p anc_i 0)
	  else
	    Image.set_matrix_path_energy img (Matrix.explicit_make (Matrix.get_down !curs_p) anc_i (new_j-1));
	  anc_e:= Matrix.get_right !anc_e;
	  anc_p:= Matrix.get_right !anc_p;
	end 
      else
	begin
	  for k = anc_j to new_j-1 do
	    anc_e:= Matrix.get_up !anc_e;
	    anc_p:= Matrix.get_up !anc_p
	  done;
	  if new_j>anc_j+1 then 
	    Image.set_matrix_path_energy img (Matrix.explicit_make (Matrix.get_down (Matrix.get_left !curs_p)) (anc_i-1) (new_j-1))
	  else 
	    if new_j<anc_j then
	      failwith("Erreur qÃ¸ui n'aurait pas dÃ» arriver dans path.ml")
	end
    in
      set !t 1 (deb-1);
      set !t 1 deb;
      set !t 1 (deb+1);
      let i=ref 0 
      and ancien_j = ref deb in
	while !i<w-1 && !next_d<= !next_f do	  
	  if !next_d<0 then d:=0
	  else d:= !next_d;
	  if !next_f>=h then f:=h-1
	  else f:= !next_f;
	  update_cursors true curs_e curs_p !i !ancien_j !d; (* initialisation des curseurs *)
	  ancien_j:= !d;
	  next_d:=h-1;
	  next_f:=0;
	  incr i;
	  let ancien_c_e= !curs_e
	  and ancien_c_p= !curs_p in
	    
	    for j= !d to !f do
	      if !t.(j) = !i then
		begin
		  update_cursors false curs_e curs_p !i !ancien_j j;
		  ancien_j:=j;
		  if update_pix_H eval_p !curs_p !curs_e !i j then
		    begin
		      set !next_t (!i+1) (j-1);
		      set !next_t (!i+1) j;
		      set !next_t (!i+1) (j+1);
		      if (j-1)< !next_d then next_d:=j-1;
		      if (j+1)> !next_f then next_f:=j+1
		    end;
		end	      
	    done;
	    curs_e:=ancien_c_e;
	    curs_p:=ancien_c_p;
	    ancien_j:= !d;
	    change_t ();
	done
and update_pix_H p act_p act_e i j =
  let a = try p i (j-1) with _ -> Types.path_energy_inf
  and b = try p i j     with _ -> Types.path_energy_inf
  and c = try p i (j+1) with _ -> Types.path_energy_inf 
  in
  let mini = (min (min a b) c) +. (Matrix.get_val act_e) in
    if mini<> Matrix.get_val act_p then
      begin 
        Matrix.set_val act_p mini;
	true;
      end
    else
      false
;;

let update_path img p = 
  match p with
    |Types.HPath(deb,_) -> update_line_H img deb
    |Types.VPath(deb,_) -> update_line_H img deb
;;
*)
