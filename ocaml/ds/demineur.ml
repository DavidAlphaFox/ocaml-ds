
open Graphics

let col = 20
let lig = 10
let pas = 30

let () =
  open_graph (Printf.sprintf " %dx%d" (pas * col) (pas * lig));
  set_color black;
  for i = 0 to col - 1 do
    let x = i * pas in
    moveto x 0; lineto x (pas * lig);
  done;
  for j = 0 to lig - 1 do
    let y = j * pas in
    moveto 0 y; lineto (pas * col) y
  done

let () = at_exit close_graph

type cell = 
  | Cache of bool * bool (* bombe / drapeau *)
  | Decouvert

let cells = Array.create_matrix col lig (Cache (false,false))

let cell i j = cells.(i).(j)

let nbb = 20

let bombe i j = match cell i j with
  | Cache (b,_) -> b
  | Decouvert -> false

let () = 
  Random.self_init ();
  let k = ref 0 in
  while !k < nbb do
    let i = Random.int col in
    let j = Random.int lig in
    if not (bombe i j) then begin
      cells.(i).(j) <- Cache (true, false);
      incr k
    end
  done

let colorie c i j =
  set_color c;
  let x = i * pas + 2 in
  let y = j * pas + 2 in
  fill_rect x y (pas - 4) (pas - 4)
    
let gris = rgb 200 200 200

let () =
  for i = 0 to col - 1 do
    for j = 0 to lig - 1 do
      colorie gris i j
    done
  done

let nb_drapeaux = ref 0
let nb_decouvert = ref 0

exception Perdu
exception Gagne

let autour i j f =
  let f i' j' = if i' >= 0 && i' < col && j' >= 0 && j' < lig then f i' j' in
  f (i-1) j;
  f (i-1) (j+1);
  f i (j+1);
  f (i+1) (j+1);
  f (i+1) j;
  f (i+1) (j-1);
  f i (j-1);
  f (i-1) (j-1)

let rec decouvre i j = 
  incr nb_decouvert;
  colorie white i j;
  let n = ref 0 in
  autour i j (fun i' j' -> if bombe i' j' then incr n);
  cells.(i).(j) <- Decouvert;
  if !n > 0 then begin 
    set_color black;
    moveto (i * pas + pas/3) (j * pas + pas/3);
    draw_char (Char.chr (Char.code '0' + !n))
  end else 
    autour i j decouvre_si_cache

and decouvre_si_cache i j = match cell i j with
  | Cache _ -> decouvre i j 
  | Decouvert -> ()

let click (x,y) =
  ignore (wait_next_event [Button_up]);
  let i = x / pas in
  let j = y / pas in
  match cell i j with
    | Cache (true, _) -> raise Perdu
    | Cache (false, _) -> decouvre i j
    | Decouvert -> ()

let dessine_drapeau i j =
  let x = i * pas in
  let y = j * pas in
  set_color red;
  fill_poly [| x+2, y+2; x+pas-4,y+pas/2; x+2,y+pas-4 |]

let pose_drapeau (x,y) =
  let i = x / pas in
  let j = y / pas in
  match cell i j with
    | Cache (b, false) -> 
	dessine_drapeau i j; incr nb_drapeaux; cells.(i).(j) <- Cache (b, true)
    | Cache (b, true) -> 
	colorie gris i j; decr nb_drapeaux; cells.(i).(j) <- Cache (b, false)
    | Decouvert -> ()

let dessine_bombe i j =
  let x = i * pas in
  let y = j * pas in
  set_color blue;
  fill_circle (x+pas/2) (y+pas/2) (pas/2-5)

let revele () =
  for i = 0 to col - 1 do
    for j = 0 to lig - 1 do
      if bombe i j then dessine_bombe i j
    done
  done;
  ignore (wait_next_event [Key_pressed])

let () = 
  try
    while true do
      let st = wait_next_event [Button_down; Key_pressed] in
      if st.button then click (mouse_pos ());
      if st.keypressed then begin
	let c = st.key in
	if c = 'q' then raise Exit else pose_drapeau (mouse_pos ())
      end;
      if !nb_drapeaux = nbb && !nb_drapeaux + !nb_decouvert = col * lig then 
	raise Gagne
    done
  with 
    | Perdu -> revele (); print_endline "perdu"; exit 0
    | Gagne -> revele (); print_endline "gagné"; exit 0
    | Exit -> exit 0


