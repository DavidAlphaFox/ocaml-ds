
open Graphics

let pas = 60
let rayon = pas/2 - 2

let () = 
  open_graph " 900x600";
  set_window_title "Caml Same";
  set_color black;
  fill_rect 0 0 900 600

type couleur = Rouge | Bleu | Jaune | Rien

let couleur_aleatoire () = match Random.int 3 with
  | 0 -> Rouge
  | 1 -> Bleu
  | 2 -> Jaune
  | _ -> assert false

let grille =
  Array.init 15 (fun _ -> Array.init 10 (fun _ -> couleur_aleatoire ()))

let couleur i j =
  if i >= 0 && i < 15 && j >= 0 && j < 10 then grille.(i).(j) else Rien

let dessine () =
  for i = 0 to 14 do
    for j = 0 to 9 do
      set_color 
	(match grille.(i).(j) with 
	   | Rouge -> red 
	   | Bleu -> blue 
	   | Jaune -> yellow 
	   | Rien -> black);
      fill_circle (pas * i + pas/2) (pas * j + pas/2) rayon
    done
  done

let tasse_tableau garde t =
  let n = Array.length t in
  let swap i j = let tmp = t.(i) in t.(i) <- t.(j); t.(j) <- tmp in
  let d = ref 0 in (* première place disponible *)
  for i = 0 to n - 1 do
    if garde t.(i) then begin swap i !d; incr d end
  done

let tasse () =
  for i = 0 to 14 do
    tasse_tableau (fun c -> c <> Rien) grille.(i)
  done;
  tasse_tableau (fun c -> c.(0) <> Rien) grille

let score = ref 0

let supprime_region c i j =
  let s = ref 0 in
  let rec enleve i j =
    if couleur i j = c then begin
      incr s;
      grille.(i).(j) <- Rien;
      enleve (i+1) j;
      enleve (i-1) j;
      enleve i (j+1);
      enleve i (j-1)
    end
  in
  enleve i j;
  score := !score + (!s - 2) * (!s - 2);
  dessine ();
  tasse ();
  dessine ()

let click i j =
  let c = couleur i j in
  if c <> Rien && (couleur (i-1) j = c || couleur (i+1) j = c ||
		   couleur i (j-1) = c || couleur i (j+1) = c) 
  then
    supprime_region c i j

let plus_de_region () =
  try
    for i = 0 to 13 do
      for j = 0 to 8 do
	let c = grille.(i).(j) in
	if c <> Rien && (c = grille.(i+1).(j) || c = grille.(i).(j+1)) then
	  raise Exit
      done
    done;
    true
  with Exit -> 
    false

let fini () = 
  if grille.(0).(0) = Rien then begin 
    score := !score + 1000; true
  end else
    plus_de_region ()

let () = 
  dessine ();
  while true do
    let st = wait_next_event [Key_pressed; Button_down] in
    if st.keypressed then exit 0;
    if st.button then begin
      let st = wait_next_event [Button_up] in
      let dx = st.mouse_x mod pas - pas/2 in
      let dy = st.mouse_y mod pas - pas/2 in
      if dx * dx + dy * dy <= rayon * rayon then begin
	let i = st.mouse_x / pas in
	let j = st.mouse_y / pas in
	click i j;
	if fini () then begin
	  Printf.printf "score = %d\n" !score; flush stdout;
	  ignore (read_key ());
	  exit 0
	end
      end
    end
  done

