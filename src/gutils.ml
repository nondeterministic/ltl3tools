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

(* get all unique states from a list of transitions, alldelta *)
   
let get_states alldelta =
  remove_doubles ((List.map (fun (a,b,c) -> a) alldelta) @
                    (List.map (fun (a,b,c) -> c) alldelta))

(*s [succ_states] takes a list of transitions (of an alternating Buchi
  automaton) and some state [s] $\in LTL$, and returns all immediate
  successor states of [s], i.e., all states reachable via taking one
  transition only. *)
          
let succ_states t s =
  let rt = List.filter (fun (src,_,_) -> 
                          if s = src then 
			    true 
                          else 
			    false) t in
  let my = List.filter (fun (_,_,dst) -> 
			  if (dst <> s) then 
			    true 
			  else 
			    false) rt in
    List.map (fun (_,_,dst) -> dst) my
      
(*s Function returns $true$ if state [q] $\in LTL$ is reachable in a
   list of transitions [t] with initial state [i] $\in LTL$, otherwise
   $false$. *)

let rec has_path t i q =
  if i = q then 
    true
  else 
    let r = succ_states t i in
      List.mem true (List.map (fun x -> has_path t x q) r)
	
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
