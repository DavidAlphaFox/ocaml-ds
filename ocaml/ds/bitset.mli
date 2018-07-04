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

(* This module provides a highly efficient implementation of sets of integers
   when the elements can be represented as bits of machine integers,
   i.e. when elements belong to the closed interval [0..word_size-2].

   For greater efficiency, the functions [add], [remove], [singleton]
   and [mem] assume the element to be in the interval [0..word_size-2];
   If needed, write your own wrappers around these functions to perform
   the checks. *)

include Set.S with type elt = int

val max_value : int
  (* the maximal possible value for an element (30 on a 32-bit architecture
     and 62 on a 64-bits architecture) *)

val full : t
  (* The set [{0,1,2,...,30}] on a 32-bit architecture
     (resp. [{0,1,2,...,62}] on a 64-bit architecture). *)

val print : Format.formatter -> t -> unit
  (* Prints a set as [{x1,x2,...,xn}]. *)

