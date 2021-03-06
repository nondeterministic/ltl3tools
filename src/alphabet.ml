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

(* When in interactive mode, do: #load "str.cma";; *)

open String
open Putils
open Mutils
open Str

let isloweroralpha c =
  List.mem c ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
		 'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
		 'u';'v';'w';'x';'y';'z';'0';'1';'2';'3';
		 '4';'5';'6';'7';'8';'9';'_']

(* Extract all the lowercase characters from a string and replace
   everything else with a comma, including white spaces. *)

let rec extr s =
  let word = ref "" in
    if String.length s = 0 then ""
    else
      begin
	try
	  for i = 0 to (String.length s - 1) do
	    let c = String.get s i in
	      if isloweroralpha c then
		word := !word^(String.make 1 c)
	      else
		word := !word^(String.make 1 ',')
	  done;
	  !word
	with Exit -> !word;
	  !word;
      end

(* Remove consecutive commas in a string. *)

let rm_cons s =
  let word = ref "" in
    try
      for i = 0 to (String.length s - 1) do
	if i <=  (String.length s - 2) then
	  begin
	    let c1 = String.get s i in
	    let c2 = String.get s (i+1) in
	      if not(c1 = ',' && c2 = ',') then
		word := (!word)^(String.make 1 c1)
	  end
	else
	  word := (!word)^(String.make 1 (String.get s i))
      done;
      !word
    with Exit -> !word

(* Remove only the trailing and initial commas in a string. *)

let rec rm_ie s =
  if String.get s 0 = ',' then
    rm_ie (String.sub s 1  ((String.length s) - 1))
 else
   if String.get s (String.length s - 1) = ',' then
     rm_ie (String.sub s 0  ((String.length s) - 1))
   else
     s

(* A wrapper around the above functions. Say, if s is "[] a || b",
   then "a,b" is returned. *)

let extract_props s =
  rm_ie (rm_cons (extr s))
    
(* Make a list of substrings contained in s. *)

let rec alphalist s =
  Str.split (Str.regexp_string ",") s
