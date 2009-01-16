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
open Putils
open Gutils
(*i*)

(*s A place where the program temporarily stores the transitions of
  the FSM. *)

let transitions = ref [ ]

(*s The input [dotfile] is a string list and contains basically the
  contents of a Graphviz DOT-file.  [parse2] goes through this, line
  by line, and sets the global variable [transitions]. *)
      
let parse2 dotfile (init_pos : int) (dotlength : int) =
  let rec parse3 (i : int) =
    if i < dotlength then
      let cur_line = ref (rmchar (List.nth !dotfile i) ' ') in
	if (safe_extfind !cur_line "->" != (-1)) then
	  begin
	    let src = (get_src_state !cur_line) in
	    let dst = (get_dst_state !cur_line) in
	    let label = (get_label !cur_line) in
	      transitions := (src, label, dst) :: !transitions;
	      parse3 (i + 1)
	  end
	else
	  parse3 (i + 1) in
    parse3 init_pos

(* s Wrapper for [parse2].  For details, see comments for
   [parse2].  *)
	      
let parse (dotfile : string list) =
  transitions := [];
  let dotfileref = ref dotfile in
    parse2 dotfileref 0 (List.length dotfile);
    !transitions
