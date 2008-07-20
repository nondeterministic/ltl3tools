(* 
   This is part of the LTL3 tools (see http://ltl3tools.sf.net/)

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

(*i*)
open Mutils
open Putils
(*i*)

(*s A place where the program temporarily stores the states of the
  FSM. *)

let states = ref [ ]

(*s A place where the program temporarily stores the transitions of
  the FSM. *)

let transitions = ref [ ]

(*s The input [dotfile] is a string list and contains basically the
  contents of a Graphviz DOT-file.  [parse2] goes through this, line
  by line, and sets the global variables [states] and [transitions],
  accordingly, then returns their contents as tuple. *)

let rec parse2 dotfile =
  match dotfile with
      [] -> (!transitions, !states)
    | h::t ->
	let hh = rmchar h ' ' in
	  if (safe_extfind hh "->" != (-1)) then (
	    let src = (get_src_state hh) in
	    let dst = (get_dst_state hh) in
	    let label = (get_label hh) in
	      transitions := !transitions @ [src, label, dst];
	      parse2 t)
	  else if ((safe_extfind hh "{" != (-1)) ||
		     (safe_extfind hh "}" != (-1))) then
	    parse2 t
	  else if (safe_extfind hh "=" = -1) ||
	    ((safe_extfind hh "=" >= 0 && safe_extfind hh "[label" >= 0)) then
	      if (safe_extfind hh "=" = -1) then (
		states := !states @ [hh];
		parse2 t)
	      else ( let upto = safe_extfind hh "[label" in
		       states := !states @ [String.sub hh 0 upto];
		       parse2 t )
	  else
	    parse2 t

(* s Wrapper for [parse2].  For details, see comments for
  [parse2].  *)

let parse dotfile =
  states := []; 
  transitions := [];
  parse2 dotfile
