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

open Printf
open Putils
open Mutils
open Alphabet

let rec print_alpha s =
  match s with
      [] -> print_endline ("")
    | h::e::t -> 
	Printf.printf ("%s,") h; print_alpha (e::t)
    | h::t -> 
	Printf.printf ("%s") h; print_alpha t

(* Gets a string from the command line, which is an LTL formula in
   LTL2BA format and returns the alphabet of the corresponding Buchi
   automaton.  That is, if the LTL formula uses the propositions "a"
   and "b", then a string "(<empty>),(b),(a),(a&&b)" is returned.  *)

let _ =
  let formula = Sys.argv.(1) in
  let alphabetlist = Alphabet.alphalist (Alphabet.extract_props formula) in
  let alphabet = 
    Putils.actions_to_alphabet (
      Mutils.powerset (
	Mutils.sort (
	  Mutils.remove_doubles (alphabetlist))))
  in
    print_alpha alphabet
