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

open Config
open Getopt
open Declarations
open String
open Putils
open Mutils
open Printf

let program_name = "nevertosymbols"

(* Outputs the symbols file format of AT&T's fsmlibrary.

   ["(!a)"; "(b)"] -> 
   (!a) 1
   (b) 2
 *)

let rec show_alpha a start =
  match a with
      [] -> Printf.printf ("");
    | h::t -> 
	Printf.printf("%s %i\n") h start;
	show_alpha t (start + 1)

let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
      while true do
	Neverp.input Neverl.token lexbuf
      done	   
  with End_of_file -> 
    let list_of_actions = powerset ( 
      sort (
	remove_doubles (
	  remove_universal_transitions (
	    extract_labels
	      !Declarations.transitions) ) ) ) in
    let alpha = actions_to_alphabet list_of_actions in
      show_alpha alpha 1

let show_help = 
  Some (
    fun () ->
      Printf.printf 
        ("%s converts a SPIN never-claim to a symbols file as used\n")
	program_name;
      Printf.printf
	("by the AT&T fsmlibrary.");
      Printf.printf
        ("\n\n");
      Printf.printf
        ("Usage: %s [OPTION] < FILE\n\n") Sys.argv.(0);
      Printf.printf
        ("Options:\n");
      Printf.printf
        (" -h, --help      Display this help information\n");
      Printf.printf
        (" -v, --version   Display version information\n\n");
      Printf.printf
        ("Report bugs to <baueran@gmail.com>.\n");
      exit 0
  )
    
let specs =
[
  ('v', "version", (Config.show_version program_name), None);
  ('h', "help", show_help, None);
  ('?', "", show_help, None)
]

let _ = 
  try
    Getopt.parse_cmdline specs print_endline;
    Declarations.transitions := [];
    Declarations.states := [];
    Declarations.alphabet := [];
    Printexc.print main ()
  with Getopt.Error (error) -> ignore (Config.anon_args program_name error)
