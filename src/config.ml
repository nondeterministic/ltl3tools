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

open Printf

let version = "0.0.1"
let copyright = "Copyright (c) 2008 by Andreas Bauer <baueran@gmail.com>"

let anon_args program_name option =
  Printf.printf ("%s: %s\n") program_name option;
  Printf.printf ("Try %s --help for more information.\n") Sys.argv.(0);
  exit 0

let show_version program_name =
  Some (
    fun () ->
      Printf.printf
	("%s %s\n\n") program_name version;
      Printf.printf
	("%s\n\n") copyright;
      print_endline 
	("This is free software; see the source for copying conditions.");
      print_endline 
	("There is NO warranty; not even for MERCHANTABILITY or FITNESS");
      print_endline 
	("FOR A PARTICULAR PURPOSE.");
      exit 0
  )
