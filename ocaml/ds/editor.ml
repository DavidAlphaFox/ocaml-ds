
open Sdl
open Video
open Event
open Format

let w = 80
let h = 40
let filename = match Sys.argv with
  | [| _; f |] when Sys.file_exists f -> f
  | _ -> eprintf "usage: editor file@."; exit 1

module Gx = struct

  let cw = 10
  let ch = 14
  let w_screen = w * cw
  let h_screen = h * ch

  let _ = Sdl.init [VIDEO]; show_cursor false

  let bpp = 16
  let flags = [SWSURFACE; ANYFORMAT] @ []

  let screen = set_video_mode w_screen h_screen bpp flags

  (* screen update *)

  let max_updates = 1 + w * h
  let num_updates = ref 0
  let dstupdates = 
    Array.init max_updates 
      (fun _ -> { rect_x = 0; rect_y = 0; rect_w = 0; rect_h = 0 })
  let srcupdates = 
    Array.init max_updates 
      (fun _ -> { rect_x = 0; rect_y = 0; rect_w = 0; rect_h = 0 })
      
  let update_screen () =
    update_rects screen !num_updates dstupdates;
  num_updates := 0

  let make_rect x y w h = { rect_x = x; rect_y = y; rect_w = w; rect_h = h }

  (* sprites *)

  type sprite = { sp_surface : surface; sp_rect : rect }

  let blit_sprite_s s spr x y =
    let dst = dstupdates.(!num_updates) in
    let src = srcupdates.(!num_updates) in
    incr num_updates;
    let r = spr.sp_rect in
    let w = r.rect_w in
    let h = r.rect_h in
    dst.rect_x <- x; dst.rect_y <- y; 
    dst.rect_w <- w; dst.rect_h <- h;
    src.rect_x <- r.rect_x; src.rect_y <- r.rect_y;
    src.rect_w <- w; src.rect_h <- h;
    blit_surface spr.sp_surface (Some src) s (Some dst)
      
  let blit_sprite = blit_sprite_s screen

  (* font *)

  let load_sprite file transp =
    let tmp = load_bmp file in
    if transp then begin
      let c = map_rgb tmp 0 0 0 (* Draw.get_pixel tmp 0 0 *) in
      set_color_key tmp [SRCCOLORKEY; RLEACCEL] c
    end;
    let spr = display_format tmp in
    free_surface tmp;
    spr

  let font = load_sprite "font.bmp" false
  let text_sprites =
    Array.init 96 
      (fun i -> { sp_surface = font; sp_rect = make_rect (10 * i) 0 cw ch })

  let draw_char c x y =
    assert (32 <= c && c <= 127);
    let c = c - 32 in
    let x = cw * x in
    let y = ch * y in
    blit_sprite text_sprites.(c) x y

  let draw_text s x y =
    let x = cw * x in
    let y = ch * y in
    for i = 0 to String.length s - 1 do
      let c = Char.code s.[i] - 32 in
      blit_sprite text_sprites.(c) (x + cw * i) y
    done

  let cursor = load_sprite "cursor.bmp" true
  let cursor_sprite = { sp_surface = cursor; sp_rect = make_rect 0 0 cw ch }
  let draw_cursor x y =
    let x = cw * x in
    let y = ch * y in
    blit_sprite cursor_sprite x y

end

(* Model *)
module File = struct

  (* line = rope of char *)
  module L = Rope.S
  type line = L.t

  (* file = rope of line *)
  module A = struct
    type t = line array
    type char = line
    let length = Array.length
    let empty = [||]
    let singleton l = [|l|]
    let append = Array.append
    let get = Array.get
    let sub = Array.sub
    let iter_range f a ofs len = for i = ofs to ofs+len-1 do f a.(i) done
    let print fmt a = Array.iter (Rope.S.print fmt) a
  end
  module C = struct let small_length = 256 let maximal_height = max_int end
  module R = Rope.Make(A)(C)

  let file = ref R.empty

  let load () =
    let buf = Buffer.create 1000000 in
    let cin = open_in filename in
    let rec read_line () = 
      match (try Some (input_char cin) with End_of_file -> None) with
	| Some c -> Buffer.add_char buf c; c == '\n' || read_line ()
	| None -> false
    in
    let rec read_lines () =
      Buffer.reset buf;
      let nl = read_line () in
      let l = L.of_string (Buffer.contents buf) in
      file := R.append !file (R.of_string [|l|]);
      if nl then read_lines ()
    in
    read_lines ();
    eprintf "%d line(s)@." (R.length !file)

  let save () = 
    eprintf "TODO: File.save@."

  let line l = R.get !file l
  let line_length l = L.length (R.get !file l)

  let nb_lines () = R.length !file

  let insert_char ln ofs c =
    let l = line ln in
    let l' = L.insert_char l ofs c in
    file := R.set !file ln l'

  let delete_char ln ofs =
    let l = line ln in
    let l' = L.delete l ofs in
    file := R.set !file ln l'

  let insert_newline ln ofs =
    let l = line ln in
    let b = L.sub l 0 ofs in
    let a = L.sub l ofs (L.length l - ofs) in
    let r = R.insert_char !file ln b in
    file := R.set r (ln + 1) a

  module KMP = Kmp.Make(struct type char = Char.t include String end)(L)

  let search_for ln ofs s =
    let nlines = R.length !file in
    let rec search ln ofs =
      if ln = nlines then raise Not_found;
      let l = line ln in
      try
	let i = KMP.search s (L.sub l ofs (L.length l - ofs)) in ln, ofs + i
      with Not_found ->
	search (ln + 1) 0
    in
    search ln ofs

end

module View = struct

  (* what is displayed on each line of the screen = line in file & offset *)
  let view = Array.init h (fun i -> 0,0)

  let reset line ofs =
    let rec fill r l ofs =
      if r < h && l < File.nb_lines () then begin
	view.(r) <- l, ofs;
	let ll = File.line_length l in
	if ofs + w < ll then fill (r+1) l (ofs+w) else fill (r+1) (l+1) 0
      end
    in
    fill 0 line ofs

  let cursor_x = ref 0
  let cursor_y = ref 0
  let update_cursor () = Gx.draw_cursor !cursor_x !cursor_y

  let update_screen_line y =
    let ln, ofs = view.(y) in
    let l = File.line ln in
    let ll = File.L.length l in
    for x = 0 to w-1 do
      let c = 
	if x <= ll - ofs - 1 then
	  let c = Char.code (File.L.get l (ofs + x)) in
	  if c < 32 || c > 127 then 32 else c 
	else
	  32
      in
      Gx.draw_char c x y
    done;
    if y = !cursor_y then update_cursor ()

  let update_all () =
    for y = 0 to h-1 do update_screen_line y done

  (* insert a character at cursor location *)
  let insert_char c =
    let x = !cursor_x in
    let y = !cursor_y in
    let ln, ofs = view.(y) in
    File.insert_char ln (ofs+x) c
    (* IMPROVE screen_line y *)

  let suppr () =
    let x = !cursor_x in
    let y = !cursor_y in
    let ln, ofs = view.(y) in
    File.delete_char ln (ofs+x)

  let backspace () =
    if !cursor_x > 0 then begin decr cursor_x; suppr () end

  let insert_newline () =
    let y = !cursor_y in
    if y < h-1 then begin
      let ln, ofs = view.(y) in
      File.insert_newline ln (ofs + !cursor_x);
      let l,o = view.(0) in
      reset l o;
      incr cursor_y;
      cursor_x := 0
    end

  let scroll_up () = let l,o = view.(1) in reset l o

  let page_down () = let l,o = view.(h-1) in reset l o

  let search_for s =
    let y = !cursor_y in
    let ln, ofs = view.(y) in
    try
      let ln, ofs = File.search_for ln (ofs + !cursor_x) s in
      eprintf "found line %d character %d@." ln ofs;
      reset ln ofs; cursor_x := 0; cursor_y := 0
    with Not_found ->
      eprintf "not found@."

  let location () =
    let y = !cursor_y in
    let ln, ofs = view.(y) in
    eprintf "line %d character %d@." ln (ofs + !cursor_x)

end

open View

(* interaction loop *)

let step = ref 0
let shift = ref false
let control = ref false
let right = ref false
let left = ref false
let up = ref false
let down = ref false
let suppr = ref false
let backspace = ref false
let azkey = ref 0
let newline = ref false
let search = ref None

let main () = 
  File.load ();
  View.reset 0 0;
  View.update_all ();
  try
    while true do 
      incr step;
      Gx.update_screen ();
      begin match poll_event () with
	| Some Quit -> 
	    raise Exit
	| Some (KeyDown k) ->
	    begin match k.key_sym.key with
	      | 27 -> search := None
	      | 113 when !control -> raise Exit (* Ctrl-Q *)
	      | 13 -> newline := true
	      | 304 -> shift := true
	      | 306 -> control := true
	      | 273 -> up := true
	      | 274 -> down := true
	      | 275 -> right := true;
	      | 276 -> left := true;
	      | n when 97 <= n && n < 97+26 -> azkey := n
	      | 127 -> suppr := true
	      | 8 -> backspace := true
	      | n -> eprintf "key %d down@." n
	    end
	| Some (KeyUp k) ->
	    begin match k.key_sym.key with
	      | 304 -> shift := false
	      | 306 -> control := false
	      | 13 -> newline := false
	      | 273 -> up := false
	      | 274 -> down := false
	      | 275 -> right := false
	      | 276 -> left := false
	      | 127 -> suppr := false
	      | n when 97 <= n && n < 97+26 -> azkey := 0
	      | 8 -> backspace := false
	      | n -> eprintf "key %d up@." n
	    end
	| _ -> 
	    ()
      end;
      let key = if !azkey > 0 && !shift then !azkey - 32 else !azkey in
      if !step mod 500 = 0 then begin
	if !search <> None && key > 0 then begin 
	  match !search with
            (* search *)
	    | Some s ->
		let s = s ^ String.make 1 (Char.chr key) in
		eprintf "searching %s@." s;
		search := Some s;
		azkey := 0;
		View.search_for s;
		update_all ()
	    | None ->
		assert false
	end else 
	(* text insertion *)
	if key > 0 && not !control && !search = None then begin
	  View.insert_char (Char.chr key);
	  if !cursor_x < w-1 then incr cursor_x;
	  update_all ()
	end else if !newline then begin
	  View.insert_newline ();
	  update_all () (* IMPROVE *)
	end else
	(* cursor move *)
	if !right && !cursor_x < w-1 then begin
	  incr cursor_x;
	  update_screen_line !cursor_y
	end else if !left && !cursor_x > 0 then begin
	  decr cursor_x;
	  update_screen_line !cursor_y
	end else if !down && !cursor_y < h-1 then begin
	  incr cursor_y;
	  update_screen_line (!cursor_y-1);
	  update_screen_line !cursor_y
	end else if !up && !cursor_y > 0 then begin
	  decr cursor_y;
	  update_screen_line (!cursor_y+1);
	  update_screen_line !cursor_y
	end else if !control && key = 122 then begin (* scroll up *)
	  View.scroll_up (); update_all ()
	end else if !control && key = 118 then begin (* page down *)
	  View.page_down (); update_all ()
	end else if !control && key = 115 then begin (* start search *)
	  search := Some ""; azkey := 0
	end else if !control && key = 108 then begin (* location *)
	  location (); azkey := 0
	end else if !control && key = 108 then begin (* location *)
	  location (); azkey := 0
	end else if !control && key = 97 then begin (* beg of line *)
	  cursor_x := 0; azkey := 0; update_screen_line !cursor_y
	end else if !control && key = 101 then begin (* end of line *)
	  cursor_x := w-1; azkey := 0; update_screen_line !cursor_y
	end else
	(* deletion *)
        if !suppr || !control && key = 100 then begin
	  View.suppr (); update_all () (* IMPROVE *)
	end else if !backspace then begin
	  View.backspace (); update_all () (* IMPROVE *)
	end
      end
    done
  with Exit ->
    ()

let _ = try main (); Sdl.quit () with e -> Sdl.quit (); raise e
