(* 
   ltl2mon - converts an LTL formula into a FSM
   Copyright (c) 2008-2009 Andreas Bauer <baueran@gmail.com>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

(* Gutils - general graph helper functions *)

open List
open Mutils
open Printf
open Hashtbl

(* Get all unique states from a list of transitions, alldelta. *)
  
let get_states alldelta =
  remove_doubles ((List.map (fun (a,b,c) -> a) alldelta) @
                    (List.map (fun (a,b,c) -> c) alldelta))

(*s [succ_states] takes a list of transitions and some state [s], and
  returns all immediate successor states of [s], i.e., all states
  reachable via taking one transition only. *)
    
let succ_states t s =
  (* First identify all transitions which have s as starting state as
     there may be more than one. *)
  let rt = ref (List.filter (fun (src,_,_) ->
                               if s = src then
				 true
                               else
				 false) t) in
    (* Then get for each of those transitions with s as starting state
       the immediate successor (and throw away self-loops).  *)
  let my = ref (List.filter (fun (_,_,dst) ->
			       if (dst <> s) then
				 true
			       else
				 false) !rt) in
    List.map (fun (_,_,dst) -> dst) !my
      
(*s Function returns $true$ if state [q] is reachable in a list of
  transitions [t] with initial state [i], otherwise $false$. A state
  is always reachable from itself even without a self-loop! This is
  basically an iterative DFS. *)
      
let has_path t p q =
  let visited_h = Hashtbl.create 500 in
    Hashtbl.add visited_h p p;
    let succs = ref (succ_states t p) in
      while (!succs <> []) do
	let w = ref (List.hd !succs) in
	  Hashtbl.add visited_h !w !w;
	  succs := List.tl !succs;
 	  List.iter
	    (fun u ->
	       if not (Hashtbl.mem visited_h u) then
		 begin
		   Hashtbl.add visited_h u u;
		   succs := u :: (!succs)
		 end)
 	    (succ_states t !w);
      done;
      if Hashtbl.mem visited_h q then
	true
      else
	false
	  
(*s This function ``prunes away'' unreachable states and their
  respective transitions, where [t] is a list of transitions of an
  alternating Buchi automaton, [i] $\in LTL$ is the initial state of
  the automaton, and [s] a list of states of the automaton, where
  each $s_i \in s$ is an LTL formula.  *)

let prune_transitions t i =
  (* first, compute a set of all reachable states: *)
  let rs = ref (List.filter (fun s -> has_path t i s) (get_states t)) in
  (* then remove all transitions which do not cover any of those
     states: *)
    List.filter (fun (s,_,_) -> 
                   if (List.mem s !rs) then 
		     true 
                   else 
		     false) t

(* For a given state, state, and an action, action, get_dest returns
   the target state, if it exists.  If none exists, the string "-1" is
   returned. The list of all transitions is stored in alldelta and
   contains of triplets of the form (state, action, state).  Notice,
   we assume that the transition table is deterministic.  Notice, this
   is a less generic version of Minimise.delta!  MAKE THIS FUNCTION
   THROW, RATHER THEN RETURN -1!*)

let get_dest (state : string) (action : string) alldelta =
  try
    match List.find (fun ((a : string), (b : string), _) ->
                       if (a = state) then
			 if (b = action) then
                           true
			 else
			   false
                       else
                         false) alldelta
    with (_, _, w) -> w
  with Not_found -> "-1"
