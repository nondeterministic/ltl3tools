(* 
   ltl2mon - converts an LTL formula into a FSM
   Copyright (c) 2008 Andreas Bauer <baueran@gmail.com>

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

open List
open Mutils
open Printf

(* get all unique states from a list of transitions, alldelta *)
   
let get_states alldelta =
  remove_doubles ((List.map (fun (a,b,c) -> a) alldelta) @
                    (List.map (fun (a,b,c) -> c) alldelta))

(*s [succ_states] takes a list of transitions and some state [s], and
  returns all immediate successor states of [s], i.e., all states
  reachable via taking one transition only. *)
    
let succ_states t s =
  (* First identify all transitions which have s as starting state as
     there may be more than one. *)
  let rt = List.filter (fun (src,_,_) -> 
                          if s = src then 
			    true 
                          else 
			    false) t in
    (* Then get for each of those transitions with s as starting state
       the immediate successor (and throw away self-loops).  *)
  let my = List.filter (fun (_,_,dst) -> 
			  if (dst <> s) then 
			    true 
			  else 
			    false) rt in
    List.map (fun (_,_,dst) -> dst) my


let rec has_path2 t i q visited =
  if i = q then
    true
  else
    (* First, get all possible successors. *)
    let all_successors = succ_states t i in
    (* Now, remove all successors, already visited. *)
    let new_successors = 
      List.filter (fun x -> (List.mem x visited) = false) all_successors in
      match new_successors with
	  [] -> false
	| _  -> 
	    List.exists 
	      (* Add the new successors to the list of previously
		 visited states. *)
	      (fun x -> has_path2 t x q (new_successors @ visited)) 
	      new_successors

(*s Function returns $true$ if state [q] is reachable in a list of
  transitions [t] with initial state [i], otherwise $false$. *)

let has_path t i q =
  has_path2 t i q []
	      
(*s This function ``prunes away'' unreachable states and their
  respective transitions, where [t] is a list of transitions of an
  alternating Buchi automaton, [i] $\in LTL$ is the initial state of
  the automaton, and [s] a list of states of the automaton, where
  each $s_i \in s$ is an LTL formula.  *)

let prune_transitions t i =
  (* first, compute a set of all reachable states: *)
  let rs = List.filter (fun s -> has_path t i s) (get_states t) in
  (* then remove all transitions which do not cover any of those
     states: *)
    List.filter (fun (s,_,_) -> 
                   if (List.mem s rs) then 
		     true 
                   else 
		     false) t

(* For a given state, state, and an action, action, get_dest returns
   the target state, if it exists.  If none exists, the string "-1" is
   returned. The list of all transitions is stored in alldelta and
   contains of triplets of the form (state, action, state).  Notice,
   we assume that the transition table is deterministic.  MAKE THIS
   FUNCTION THROW, RATHER THEN RETURN -1!*)

let get_dest state action alldelta =
  try
    match List.find (fun (a,b,c) ->
                       if (a = state && b = action) then 
                         true 
                       else 
                         false) alldelta
    with (u, v, w) -> w
  with Not_found -> "-1"
