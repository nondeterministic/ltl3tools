(* 
   This is part of the LTL3 tools (see http://ltl3tools.sf.net/)

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

(*i*)
open Mutils
open Gutils
(*i*)

(*s [merge_final] takes a list of transitions containing the states
  and transitions of a monitor, i.e., a 3-valued Moore machine, and
  returns the same Moore machine, but merges all $\top$ and $\bot$
  states to one, respectively.  The $?$-states remain unaffected.
  Notice, this is not an optimal minimization, but rather a
  preparation for it. *)

let merge_final transitions = 
  let left_merged = 
    List.map (fun ((a1,a2), b, c) ->
		if ((int_of_string a1) < 0) then
		  (("-1","1"), b, c)
		else if ((int_of_string a2) < 0) then
		  (("1","-1"), b, c)
		else
		  ((a1,a2), b, c)
	     ) transitions in
    remove_doubles 
      (List.map (fun (a, b, (c1,c2)) ->
		   if ((int_of_string c1) < 0) then
		     (a, b, ("-1","1"))
		   else if ((int_of_string c2) < 0) then
		     (a, b, ("1","-1"))
		   else
		     (a, b, (c1,c2))
		) left_merged)

(*s Returns [true] if the output symbol associated with state [(a, b)]
  is the same as that of state [(c, d)], or if both states represent
  ?-states. *)

let initially_the_same (a, b) (c, d) =
  if (int_of_string a >= 0 && int_of_string c >= 0
      && int_of_string b >= 0 && int_of_string d >= 0)
    || ((a = c) && (b = d)) then true else false

(* Takes a state [state], an action [action], and a list of
   transitions [transitions], then returns the function defined by the
   transitions, $\delta(state, action)$.  Notice, this is a
   polymorphic variant of Gutils.get_dest, hence less efficient! *)

let delta state action transitions =
  match List.find (fun (a, b, _) -> 
                     if (a = state) then
		       if (b = action) then 
			 true 
		       else 
			 false
                     else false) transitions  
  with (_,_,c) -> c

(* The following functions take responsibility to determine the set of
   marked and unmarked states of the well-known FSM minimisation
   algorithm.  *)

let rec find_new_marked (s1, s2) trans marked sigma =
  match sigma with
      [] -> marked
    | a::srest ->
        let new_pair = ((delta s1 a trans), (delta s2 a trans)) in
          if List.mem new_pair marked then
            find_new_marked (s1, s2) trans ((s1, s2) :: marked) srest
          else
            find_new_marked (s1, s2) trans marked srest

let rec marked_states2 trans marked unmarked sigma =
  match marked, unmarked with
      marked, [] -> marked
    | marked, (s1, s2)::umrest ->
        let new_marked = find_new_marked (s1, s2) trans marked sigma in
          marked_states2 trans new_marked umrest sigma

(*s This function takes a list of transitions, [trans], an alphabet,
  [sigma], and returns a list of state tuples, such that each tuple
  $(x, y)$ consists of two states that are distinguishable in the
  sense that different output symbols are associated with them. See
  also function [unmarked_states]. *)
            
let rec marked_states trans marked unmarked sigma =
  let new_marked = marked_states2 trans marked unmarked sigma in
    if (new_marked = marked) then
      marked
    else
      marked_states trans new_marked (diff unmarked new_marked) sigma

(*s This function takes a list of transitions, [trans], an alphabet,
  [sigma], and returns a list of state tuples, such that each tuple
  $(x, y)$ consists of two states that are INdistinguishable in the
  sense that their output values are the same and that they lead to
  the same states on the same input symbol.  See also function
  [marked_states]. *)

let unmarked_states trans sigma = 
  let states = ref (Gutils.get_states trans) in
  let all_pairs = unlist (cartesian !states !states) in

  let init_unmarked = List.filter (fun (s1, s2) ->
				     if (initially_the_same s1 s2) then
				       true
				     else
				       false) all_pairs in
  let init_marked = diff all_pairs init_unmarked in
  let marked = marked_states trans init_marked init_unmarked sigma in
    diff all_pairs marked

(*s [state] is a state (x, y), and [unmarked_states] a list of
  unmarked state pairs, such that the function returns a list of type
  [(a', a') list] containing all equivalent states to (x, y), where of
  course (x,y) is part of this list. *)

let find_eq_class state unmarked_states =
  let eq = List.filter (fun (s1, s2) ->
                          if (s1 = state || s2 = state) then true else false
                       ) unmarked_states in
  let eq_nodoubles = remove_double_pairs eq in
  let eq_list = unpair_list eq_nodoubles in
    remove_doubles (sort eq_list)

(* Returns a list of equivalent states, see also function
   [find_eq_class]. *)

let eq_states transitions sigma =
  let trans = merge_final transitions in
  let states = ref (Gutils.get_states trans) in
  let unmarked_state_pairs = unmarked_states trans sigma in
    remove_doubles 
      (sort
	 (List.map 
            (fun (s1, s2) -> 
               find_eq_class (s1, s2) unmarked_state_pairs
            ) !states))

(* The argument [eq_state] is a list of states, [(a', a') list], which
   represents an equivalence class.  The function then returns a list
   of transitions for this equivalence class, which corresponds to
   part of the total transition relation of the minimised
   automaton. *)

let eq_trans eq_state transitions sigma =
  let transitions = ref (merge_final transitions) in
  let local_unmarked_states = unmarked_states !transitions sigma in
  let (s1, s2) = List.hd eq_state in
    List.map 
      (fun a ->
	 let (x, y, z) = 
	   List.find (fun (p, act, q) ->
			if ((p = (s1, s2)) && (act = a)) then 
			  true 
			else 
			  false) !transitions in
	   (* Result transition: *)
	   ((s1, s2), a, List.hd (find_eq_class z local_unmarked_states))
      ) sigma
      
let minimise transitions sigma =
  sort
    (remove_doubles
       (unlist
	  (List.map (fun state_list ->
		       eq_trans state_list transitions sigma
		    ) (eq_states transitions sigma))))
