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

open Getopt
open String
open Config
open Declarations
open Putils
open Mutils

let program_name = "nevertofsm"

let extalpha = ref []

(* Gets a string representing a numberical value, possibly with a trailing 0, *)
(* and returns that value as a string without trailing 0. *)

let normalise num = string_of_int (int_of_string num)

(* Some printing functions: *)

let rec show_trans transitions =
  match transitions with
      [] -> Printf.printf("")
    | (p, a, q)::t -> 
        Printf.printf ("%s %s %s\n") (normalise p) (normalise q) a;
        show_trans t

let rec show_states states =
  match states with
      [] -> Printf.printf("")
    | h::t -> Printf.printf ("%s\n") (normalise h); show_states t

 (* LTL2BA gives transitions such as (!b) meaning that the transition
    is enabled for all actions that do not contain "b".  Hence (!b)
    can be a shortcut for (a), (<empty>), etc.  Hence unfold_trans
    "unfolds" all such shortcuts into proper transitions based on
    alphabet alpha.  transitions is the usual list of triples of the
    form (state, action, state), and alpha is a list of strings
    containing the alphabet.  For example, "unfold_trans ... ["a";
    "b"; "a&&b"; "<empty>"]" would return a new transition table of
    the above form, where all transitions only contain labels that
    appear in the alphabet.  All occurrences of (!b), etc. are
    gone. *)

let unfold_trans transitions alpha =
  let rec utr transitions =
    match transitions with
	[] -> []
      | (q, a, p)::t ->
          (List.map
             (fun c -> (q, c, p)) (sigma_filter (get_action_list a) alpha))
          @ utr t in
    utr transitions
	  
(* LTL2BA gives a (1) transition to mean a universal transition.  See
   comment for unfold_trans; unfold_sigma replaces all (1)
   transitions with proper transitions from the alphabet. *)
          
let unfold_sigma transitions alpha =
  let rec usr transitions =
    match transitions with
	[] -> []
      | (q, a, p)::t -> 
          if (a = "(1)") then 
            (List.map (fun c -> (q, c, p)) alpha) @ usr t
          else (q, a, p) :: usr t in
    usr transitions

(* This function is only for debugging purposes, and not actually
   called by the rest of the code.  *)

let rec show_alpha a =
  match a with
      [] -> Printf.printf ("");
    | h::t ->
        Printf.printf("%s") h;
        if (List.length t >= 1) then
          Printf.printf(", ");
        show_alpha t
	  
let unprocessed_trans = ref []

let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
      while true do
	Neverp.input Neverl.token lexbuf
      done	   
  with End_of_file -> 
    if (List.length !extalpha > 0) then
      Declarations.alphabet := 
	Str.split (Str.regexp_string ",") 
	  (List.hd !extalpha)
    else
      Declarations.alphabet := 
	actions_to_alphabet (
	  powerset ( 
	    sort (
	      remove_universal_transitions (
		remove_doubles (
		  extract_labels
		    !Declarations.transitions) ) ) ) );
    unprocessed_trans := 
      unfold_trans
	(unfold_sigma
	   !Declarations.transitions !Declarations.alphabet)
	!Declarations.alphabet;
    let processed_trans = remove_doubles !unprocessed_trans in
      show_trans processed_trans;
      show_states !Declarations.states;
      exit 0
	
let show_help = 
  Some (
    fun () ->
      Printf.printf 
	("%s converts a SPIN never-claim to AT&T's fsmlibrary format.")	
	program_name;
      Printf.printf
	("\n\n");
      Printf.printf
	("Usage: %s [OPTION] < FILE\n\n") Sys.argv.(0);
      Printf.printf
	("Options:\n");
      Printf.printf
	(" -a, --alph \"ALPHABET\"   Define alphabet\n");
      Printf.printf
	(" -h, --help              Display this help information\n");
      Printf.printf
	(" -v, --version           Display version information\n\n");
      Printf.printf
	("Report bugs to <baueran@gmail.com>.\n");
      exit 0
  )
    
let specs =
[
  ('a', "alpha", None, (Getopt.append extalpha));
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
