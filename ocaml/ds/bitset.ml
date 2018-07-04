(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

type elt = int

type t = int (* bit vector, including the sign bit *)

let empty = 0

let full = -1

let is_empty s = s == 0

let bit i = 1 lsl i

let singleton i = 1 lsl i

let add i s = (1 lsl i) lor s

let of_list = List.fold_left (fun s x -> add x s) empty

let remove i s = (lnot (1 lsl i)) land s

let mem i s = ((1 lsl i) land s) != 0
let find i s = if mem i s then i else raise Not_found

let union = (lor)

let inter = (land)

let diff s1 s2 = s1 land (lnot s2)

let subset s1 s2 = s1 land (lnot s2) == 0

(* TODO: improve (cf motus.ml) *)
let cardinal s =
  let rec loop n s =
    if s == 0 then n else loop (succ n) (s - (s land (-s)))
  in
  loop 0 s

(* inverse of bit i = 1 lsl i i.e. tib i = log_2(i) *)
let log2 = Array.make 255 0
let () = for i = 0 to 7 do log2.(1 lsl i) <- i done

(* assumption: x is a power of 2 *)
let tib32 x =
  if x land 0xFFFF == 0 then
    let x = x lsr 16 in
    if x land 0xFF == 0 then 24 + log2.(x lsr 8) else 16 + log2.(x)
  else
    if x land 0xFF == 0 then 8 + log2.(x lsr 8) else log2.(x)

let ffffffff = (0xffff lsl 16) lor 0xffff
let tib64 x =
  if x land ffffffff == 0 then 32 + tib32 (x lsr 32) else tib32 x

let tib =
  match Sys.word_size with 32 -> tib32 | 64 -> tib64 | _ -> assert false

let min_elt s =
  if s == 0 then raise Not_found;
  tib (s land (-s))

let choose = min_elt

(* TODO: improve? *)
let max_elt s =
  if s == 0 then raise Not_found;
  let rec loop i =
    if s land i != 0 then tib i
    else if i = 1 then raise Not_found else loop (i lsr 1)
  in
  loop min_int

let rec elements s =
  if s == 0 then [] else let i = s land (-s) in tib i :: elements (s - i)

let rec iter f s =
  if s != 0 then let i = s land (-s) in f (tib i); iter f (s - i)

let rec fold f s acc =
  if s == 0 then acc else let i = s land (-s) in fold f (s - i) (f (tib i) acc)

let rec for_all p s =
  s == 0 || let i = s land (-s) in p (tib i) && for_all p (s - i)

let rec exists p s =
  s != 0 && let i = s land (-s) in p (tib i) || exists p (s - i)

let rec filter p s =
  if s == 0 then
    0
  else
    let i = s land (-s) in
    let s = filter p (s - i) in
    if p (tib i) then s + i else s

let rec partition p s =
   if s == 0 then
    0, 0
  else
    let i = s land (-s) in
    let st,sf = partition p (s - i) in
    if p (tib i) then st + i, sf else st, sf + i

let split i s =
  let bi = 1 lsl i in
  s land (bi - 1), s land bi != 0, s land (-1 lsl (i+1))

let compare = Pervasives.compare

let equal = (==)

let max_value = Sys.word_size - 2

let print fmt s =
  Format.fprintf fmt "{";
  let rec pr = function
    | [] -> ()
    | x :: l ->
	Format.fprintf fmt "%d" x; if l <> [] then Format.fprintf fmt ",@,";
	pr l
  in
  pr (elements s);
  Format.fprintf fmt "}"

