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

(* Garsia-Wachs algorithm for optimum binary tree. 
   See TAOCP vol. 3 page 451. *)

type 'a tree =
  | Leaf of 'a
  | Node of 'a tree * 'a tree

val garsia_wachs : ('a * int) list -> 'a tree
  (** given a list of elements together with nonnegative weights, 
      say [[x1,w1; ...; xn,wn]], this algorithm builds an optimum
      binary tree for [x1,...,xn], that is a tree whose leaves
      are [x1,...,xn] in symmetric order and which minimizes
      the sum [w1*d(x1) + ... + wn*d(xn)] where [d(xi)] is the depth of 
      leaf [xi]. *)

