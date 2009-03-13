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

let rec print_s s c =
  match s with
      [] -> Printf.printf ("")
    | h::t -> 
	Printf.printf ("%s %i\n") h c; 
	print_s t (c+1)

let print_symbols s = print_s s 1

(* Gets a string from the command line, which is an LTL formula in
   LTL2BA format and returns the symbols in the format of the
   fsmlibrary. *)

let _ =
  let formula = Sys.argv.(1) in
  let alphabetlist = Alphabet.alphalist (Alphabet.extract_alpha formula) in
  let alphabet = 
    Putils.actions_to_alphabet (
      Mutils.powerset (
	Mutils.sort (
	  Mutils.remove_doubles (alphabetlist))))
  in
    print_symbols alphabet
