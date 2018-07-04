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

(*s Search algorithms, presented both functionally and imperatively. *)

(*s Functional search *)

module type FunctionalProblem = sig

  (* states *)
  type state
  val success : state -> bool

  (* moves *)
  type move
  val moves : state -> (move * state) list

  (* visited states are stored in a table using the following type
     and functions; when it is not necessary/desired to mark visited states,
     simply provide a function [mem] always returning false.
     Note: [clear] is used for iterative deepening search only. *)
  type table
  val create : unit -> table
  val add : table -> state -> unit
  val mem : table -> state -> bool
  val clear : table -> unit
end

(* [search s] searches for a successful state starting from state [s];
   if there is such a state, it returns it, together with the path from [s];
   otherwise, it raises [Not_found]. *)

(* Depth-first search *)
module FunctionalDFS(P : FunctionalProblem) : sig
  val search : P.state -> P.state * P.move list
end

(* Breadth-first search *)
module FunctionalBFS(P : FunctionalProblem) : sig
  val search : P.state -> P.state * P.move list
end

(* Iterative deepening search *)
module FunctionalIDS(P : FunctionalProblem) : sig
  val search : P.state -> P.state * P.move list
end

(*s Imperative search *)

module type ImperativeProblem = sig

  (* the state is global and imperative *)
  val success : unit -> bool

  (* moves *)
  type move
  val moves : unit -> move list
  val do_move : move -> unit
  val undo_move : move -> unit

  (* visited states are stored using the following functions:
     [add] stores the current state in the table and [mem] checks whether
     the current state is already in the table *)
  val add : unit -> unit
  val mem : unit -> bool
  val clear : unit -> unit

end

(* [search ()] searches for a successful state starting from the current state;
   if there is such a state, it returns the path from the initial state;
   otherwise, it raises [Not_found]. *)

(* Depth-first search *)
module ImperativeDFS(P : ImperativeProblem) : sig
  val iter: (P.move list -> unit) -> unit
  val search : unit -> P.move list
end

(* Breadth-first search *)
module ImperativeBFS(P : ImperativeProblem) : sig
  val search : unit -> P.move list
end

(* Iterative deepening search *)
module ImperativeIDS(P : ImperativeProblem) : sig
  val search : unit -> P.move list
end
