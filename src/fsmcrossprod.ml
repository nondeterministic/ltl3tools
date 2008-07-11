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

open Config
open Getopt
open Printf
open String
open Putils
open Mutils
open Gutils

let program_name = "fsmcrossprod"

let states = ref [ "" ]
let transitions = ref [("", "", "")]
let alphabet = ref [ "" ]
let ifiles = ref [ ]
let comb_states = ref [ ]
let colouring = ref false

(* read_file reads the contents of a file named filename, and returns
   them as a string list *)

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

(* l is a string list and contains basically the contents of a
   DOT-file. parse_dot goes through this line by line and sets the
   global variables states and transitions. *)

let rec parse_dot l =
  match l with
      [] -> (!transitions, !states)
    | h::t ->
	let hh = rmchar h ' ' in 
	  if (safe_extfind hh "->" != (-1)) then (
	    let src = (get_src_state hh) in
	    let dst = (get_dst_state hh) in
	    let label = (get_label hh) in
	      transitions := !transitions @ [src, label, dst];
	      parse_dot t)
	  else if ((safe_extfind hh "{" != (-1)) || 
		     (safe_extfind hh "}" != (-1))) then
	    parse_dot t
	  else if (safe_extfind hh "=" = -1) || 
	    ((safe_extfind hh "=" >= 0 && safe_extfind hh "[label" >= 0)) then
	      if (safe_extfind hh "=" = -1) then ( 
		states := !states @ [hh];
		parse_dot t)
	      else ( let upto = safe_extfind hh "[label" in 
		       states := !states @ [String.sub hh 0 upto];
		       parse_dot t )
	  else
	    parse_dot t

(* THIS IS FOR ***DFA*** ONLY! *)

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

(* This function is never called, it is for debugging only. *)

let rec show_trans transitions =
  match transitions with
      [] -> Printf.printf("")
    | (p, a, q)::t ->
	Printf.printf ("%s -> %s [label = \"%s\"]\n") p q a;
	show_trans t

(* Add combined states to a global data structure, in case, we want to
   print the states later separately *)

let rec add_comb_states alldelta_product_automaton =
  match alldelta_product_automaton with
      [] -> comb_states := !comb_states
    | ((p1,p2), a, (q1,q2))::t -> 
	comb_states := !comb_states @ [(p1,p2); (q1,q2)];
	add_comb_states t

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

let rec show_prod alldelta_product_automaton =
  match alldelta_product_automaton with
      [] -> Printf.printf("")
    | ((p1,p2), a, (q1,q2))::t ->
	Printf.printf
	  ("\"(%s, %s)\" -> \"(%s, %s)\" [label = \"%s\"]\n") p1 p2 q1 q2 a;
	show_prod t
	     
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

let specs =
[
  ('f', "file", None, (Getopt.append ifiles));
  ('c', "colour", (Getopt.set colouring true), None);
  ('v', "version", (Config.show_version program_name), None);
  ('h', "help", show_help, None);
  ('?', "", show_help, None)
]

let show_error msg =
  Printf.printf ("%s: %s\n") program_name msg;
  exit 0

let _ = 
  try
    Getopt.parse_cmdline specs print_endline;
    transitions := [];
    states := [];
    match !ifiles with
	h::t::[] ->
	  let fsm1 = (read_file h) in
	  let (trans1, states1) = parse_dot fsm1 in
	    transitions := [];
	    states := [];
	    alphabet := [];
	    let fsm2 = (read_file t) in
	      alphabet := 
		actions_to_alphabet (
		  powerset (
		    sort (
		      remove_doubles (
			extract_labels trans1))));
	      let (trans2, states2) = parse_dot fsm2 in
	      let prod = unlist (unlist (cproduct states1 states2 
					   trans1 trans2 
					   !alphabet)) in
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
    
    
