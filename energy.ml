(* direction du calcul du gradient *)
type grad_direc = GH | GV | GPB | GSB   (* horizontale, verticale, première bissectrice, seconde bissectrice *)

(* calcul du gradient dans chaque direction *)
let calc_gradient f w h i j d=
  match d with
    | GH -> 
	if i = 0 then 
	  2.0 *. ((f 1 j) -. (f 0 j))
	else if i = w-1 then
	  2.0 *. ((f (w-1) j) -. (f (w-2) j))
	else
	  ((f (i+1) j) -. (f (i-1) j))
    | GV ->
	if j = 0 then 
	  2.0 *. ((f i 1) -. (f i 0))
	else if j = h-1 then
	  2.0 *. ((f i (h-1)) -. (f i (h-2)))
	else
	  ((f i (j+1))-. (f i (j-1)))
    | GPB ->
	if i = 0 then 
	  if j = h-1 then 
	    0.0
	  else
	    2.0 *. ((f 1 (j+1)) -. (f 0 j))
	else if i = w-1 then
	  if j = 0 then
	    0.0
	  else
	    2.0 *. ((f (w-1) j) -. (f (w-2) (j-1)))
	else
	  if j=0 then
	    2.0 *. ((f (i+1) 1) -. (f i 0))
	  else if j=h-1 then
	    2.0 *. ((f i (h-1)) -. (f (i-1) (h-2)))
	  else
	    ((f (i+1) (j+1)) -. (f (i-1) (j-1)))
    | GSB ->
	if i = 0 then 
	  if j = 0 then 
	    0.0
	  else
	    2.0 *. ((f 1 (j-1)) -. (f 0 j))
	else if i = w-1 then
	  if j = h-1 then
	    0.0
	  else
	    2.0 *. ((f (w-1) j) -. (f (w-2) (j+1)))
	else
	  if j=0 then
	    2.0 *. ((f i 0) -. (f (i-1) 1))
	  else if j=h-1 then
	    2.0 *. ((f (i+1) (h-2)) -. (f i (h-1)))
	  else
	    ((f (i+1) (j-1)) -. (f (i-1) (j+1)))
;;

(* Calcul d'énergie: variante 1: on prend que les voisins gauche droite haut bas *)
let pix_energy1 img i j=
  let w = Image.width img and h = Image.height img in
  let f = calc_gradient (Image.get_intensity img) w h
  in
  let dx= f i j GH
  and dy= f i j GV 
  in
    sqrt(dx*.dx +. dy*.dy)
;;

(* Calcul d'énergie: variante 2: on prend les 8 voisins *)
let pix_energy2 img i j=
  let w = Image.width img and h = Image.height img in
  let f = calc_gradient (Image.get_intensity img) w h
  in
  let dx= f i j GH
  and dy= f i j GV
  and dpb= f i j GPB
  and dsb= f i j GSB
  in
    2.0 *. sqrt(dx*.dx +. dy*.dy) +. sqrt(dpb*.dpb +. dsb*.dsb)
;;

(* Calcul d'énergie: variante 3: juste l'intensité, pour débugger *)
let pix_energy_default img i j =
  Image.get_intensity img i j
;;

(* petite fonction d'abstraction *)
let pix_energy n = 
  match n with
    | 1 -> pix_energy1
    | 2 -> pix_energy2
    | _ -> pix_energy_default 
;;

(* calcul l'énergie de tout les pixels de la matrice, n est la variante à utiliser pour le calcul de l'énergie *)
let calc n img=
  let w = Image.width img and h = Image.height img in
  let pix_en i j = pix_energy n img i j
  in
    for i=0 to w-1 do
      for j=0 to h-1 do
	Image.set_intensity img i j (Pixel.intensity (Image.get_data img i j))   (* il faut d'abord calculer l'intensité *)
      done
    done;
    for i=0 to w-1 do
      for j=0 to h-1 do
	Image.set_energy img i j (pix_en i j)
      done
    done;
;;

(* calcul l'énergie des pixels adjacent au chemin supprimé, n est la variante à utiliser pour le calcul de l'énergie *)
let update_del_path n img p=
  let w = Image.width img and h = Image.height img in
  let maj_pix i j = (* met à jour le pixel i j *)
    if 0<=i && 0<=j && i<w && j<h then   (* < utile si le chemin passe par les anciens bords de l'image (ceux qui viennent d'être supprimé dans Image.del_path) *)
      Image.set_energy img i j (pix_energy n img i j) 
 in
  match p with
    | Types.VPath(deb,seq) -> 
	begin
	  let x=ref deb and y=ref 0 in 	     
	    maj_pix !x !y;      (* on met à jour le pixel du chemin *)
	    maj_pix (!x-1) !y;  (* et son voisin de gauche: les deux suceptibles d'avoir changé *)
	    List.iter (fun d ->
			 begin
			   match d with
			     | Types.L -> x:= !x-1; incr y
			     | Types.F ->           incr y
			     | Types.R -> x:= !x+1; incr y
			 end;
			 maj_pix !x !y;     (* on met à jour le pixel du chemin *)
			 maj_pix (!x-1) !y  (*et son voisin de gauche: les deux suceptibles d'avoir changé *)
		      ) seq
	end
    | Types.HPath(deb,seq) -> (* pareil que pour VPath *)
	begin
	  let x=ref 0 and y=ref deb in  
	    maj_pix !x !y;
	    maj_pix !x (!y-1);
	    List.iter (fun d ->
			 begin
			   match d with
			     | Types.L -> y:= !y+1; incr x
			     | Types.F ->           incr x
			     | Types.R -> y:= !y-1; incr x
			 end;
			 maj_pix !x !y;
			 maj_pix !x (!y-1)
		      ) seq;
	end
;;

(* calcul l'énergie des pixels adjacents au chemin ajouté, idem que del_path *)
let update_add_path n img p=
  let w = Image.width img and h = Image.height img in
  let maj_pix i j =
    if 0<=j && j<h && 0<=i && i<w then
      Image.set_energy img i j (pix_energy n img i j);    
 in
  match p with
    | Types.VPath(deb,seq) -> 
	begin
	  let x=ref deb and y=ref 0 in  	    
	    maj_pix (!x-1) !y;
	    maj_pix !x !y;
	    maj_pix (!x+1) !y; (* il faut aussi s'interesser au voisin de droite *)
	    List.iter (fun d ->
			 begin
			   match d with
			     | Types.L -> x:= !x-1; incr y
			     | Types.F ->           incr y
			     | Types.R -> x:= !x+1; incr y
			 end;
			 maj_pix (!x-1) !y;
			 maj_pix !x !y;
			 maj_pix (!x+1) !y			 
		      ) seq
	end
    | Types.HPath(deb,seq) -> 
	begin
	  let x=ref 0 and y=ref deb in 
	    maj_pix !x (!y-1); 
	    maj_pix !x !y;
	    maj_pix !x (!y+1);	    
	    List.iter (fun d ->
			 begin
			   match d with
			     | Types.L -> y:= !y+1; incr x
			     | Types.F ->           incr x
			     | Types.R -> y:= !y-1; incr x
			 end;
			 maj_pix !x (!y-1);
			 maj_pix !x !y;
			 maj_pix !x (!y+1)			 
		      ) seq
	end
;;

(* dessine la matrice de l'energie, pour débugger *)
let draw img x0 y0= 
  let w=Image.width img and h=Image.height img in
  let color_matrix = Array.make_matrix h w (Graphics.rgb 0 0 0) in
  let max_energy = ref (-10000.0) 
  and min_energy = ref (10000.0) in
    for j=0 to h-1 do
      for i = 0 to w-1 do
	let v = Image.get_energy img i j in 
	  if !max_energy < v then max_energy:= v;
	  if !min_energy > v then min_energy:= v
      done
    done;
    for j=0 to h-1 do
      for i = 0 to w-1 do
	color_matrix.(h-1-j).(i) <- 
	  let c = int_of_float(((Image.get_energy img i j-. !min_energy) /. (!max_energy-. !min_energy)) *. 255.0) in
	    Graphics.rgb c c c
      done
    done;
    Graphics.draw_image (Graphics.make_image color_matrix) x0 y0;
;;
