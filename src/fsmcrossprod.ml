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
open Config
open Getopt
open Printf
open String
open Putils
open Mutils
open Gutils
open Dot
(*i*)

(*s The program's name. *)

let program_name = "fsmcrossprod"

(*s A list of strings which contains the file names of the input
  files. *)

let ifiles = ref [ ]

(*s The combined states of both input FSM after the cross product was
  taken. *)

let comb_states = ref [ ]

(*s True if the states of the cross product should be coloured,
  otherwise false. *)

let colouring = ref false

(*s [read_file] reads the contents of a file named [filename], and
  returns them as a string list *)

let read_file filename =
  if Sys.file_exists filename
  then
    let lines = ref [] in
    let l = open_in filename in
      try
	while true do
	  lines := input_line l :: !lines
	done;
 	assert false
      with End_of_file ->
	List.rev !lines
  else
    ["The file does not exist"]


(*s [cproduct] takes as input two lists of states, and two lists of
  transitions of two \textbf{deterministic} finite state machines
  (FSM), as well as their alphabet, [sigma].  Each state list is a
  list of strings representing the states' names, each [delta] is a
  list of type [string * string * string] (as the FSMs are
  deterministic), and [sigma] is basically a comma-separated list of
  actions (i.e., one long string).

  The function returns a list of type [(string * string) * string *
  (string * string)], where the pairs of strings are the new states
  and the entire entry corresponds to a single transition in the
  product automaton. *)

let rec cproduct states1 states2 delta1 delta2 sigma =
  let s1 = states1 @ ["-1"] in
  let s2 = states2 @ ["-1"] in
    List.map (fun p1 ->
		List.map (fun p2 ->
                            List.map (fun a -> 
					let q1 = get_dest p1 a delta1 in
					let q2 = get_dest p2 a delta2 in
                                          ((p1,p2), a, (q1, q2))
                                     ) sigma) s2) s1

(*s Adds the combined states of the product automaton, once built, to
  a global data structure, [comb_states]. *)

let rec add_comb_states alldelta_product_automaton =
  match alldelta_product_automaton with
      [] -> comb_states := !comb_states
    | ((p1,p2), a, (q1,q2))::t -> 
	comb_states := !comb_states @ [(p1,p2); (q1,q2)];
	add_comb_states t

(*s [show_comb_states] prints [states] on standard output using
  traffic-light colours, where [states] is a list of type [string *
  string].  Red are meant to be violating states, green are satisfying
  states, and yellow neither.  This function is basically triggered by
  the [colouring] flag, respectively the [--c] command line option. *)

let rec show_comb_states states =
  match states with
      [] -> Printf.printf("")
    | (q1,q2)::t -> 
	if ((int_of_string q1) = -1) then
	  Printf.printf ("\"(%s, %s)\" [style=filled, color=red]\n") q1 q2
	else if ((int_of_string q2) = -1) then
	  Printf.printf ("\"(%s, %s)\" [style=filled, color=green]\n") q1 q2
	else
	  Printf.printf ("\"(%s, %s)\" [style=filled, color=yellow]\n") q1 q2;
	show_comb_states t

(*s [show_prod] takes as input [alldelta_product_automaton], a list of
  type [(string * string) * string * (string * string)], representing
  the transition table of a finite state machine.  It then prints this
  on standard output using the Graphviz DOT-syntax. *)

let rec show_prod alldelta_product_automaton =
  match alldelta_product_automaton with
      [] -> Printf.printf("")
    | ((p1,p2), a, (q1,q2))::t ->
	Printf.printf
	  ("\"(%s, %s)\" -> \"(%s, %s)\" [label = \"%s\"]\n") p1 p2 q1 q2 a;
	show_prod t

(*s Displays help text and options of the program on standard output *)
	     
let show_help = 
  Some (
    fun () ->
      Printf.printf 
        ("%s returns the product for two deterministic finite-state machines\n")
	program_name;
      Printf.printf 
        ("stored in AT&T's fsmlibrary format.");
      Printf.printf
        ("\n\n");
      Printf.printf
        ("Usage: %s [OPTION] --file FILE1 --file FILE2\n\n") 
	Sys.argv.(0);
      Printf.printf
        ("Options:\n");
      Printf.printf
	(" -f, --file FILE   Define FSM input file\n");
      Printf.printf
	(" -c, --colour      Turn on colouring\n");
      Printf.printf
        (" -h, --help        Display this help information\n");
      Printf.printf
        (" -v, --version     Display version information\n\n");
      Printf.printf
        ("Report bugs to <baueran@gmail.com>.\n");
      exit 0
  )

(*s fsmcrossprod's command line options stored in a list to be
  processed by Alain Frisch's getopt. *)

let specs =
[
  ('f', "file", None, (Getopt.append ifiles));
  ('c', "colour", (Getopt.set colouring true), None);
  ('v', "version", (Config.show_version program_name), None);
  ('h', "help", show_help, None);
  ('?', "", show_help, None)
]

(*s This function is called when an exception is thrown during the
  parsing of command line parameters. *)

let show_error msg =
  Printf.printf ("%s: %s\n") program_name msg;
  exit 0

(*s The main entry for fsmcrossprod.  Handles processing of command
line parameters via Alain Frisch's getopt, calls processing functions
for reading the input files, etc. *)

let _ = 
  try
    Getopt.parse_cmdline specs print_endline;
    transitions := [];
    states := [];
    match !ifiles with
	h::t::[] ->
	  let fsm1 = (read_file h) in
	  let (trans1, states1) = Dot.parse fsm1 in
	  let fsm2 = (read_file t) in
	  let alphabet = 
	    actions_to_alphabet (
	      powerset (
		sort (
		  remove_doubles (
		    extract_labels trans1)))) in
	  let (trans2, states2) = Dot.parse fsm2 in
	  let prod = unlist (unlist (cproduct states1 states2 
				       trans1 trans2 
				       alphabet)) in
	    print_endline ("digraph G {");
	    let comb_delta = (prune_transitions prod ("0", "0")) in
	      show_prod comb_delta;
	      if (!colouring = true) then (
		add_comb_states comb_delta;
		show_comb_states (remove_doubles !comb_states)
	      );
	      print_endline ("}")
      | _ -> show_error ("Incorrect number of arguments.\n")
  with Getopt.Error (error) -> ignore (Config.anon_args program_name error)
