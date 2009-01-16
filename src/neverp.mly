/* 
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
*/

%{
open String
open Declarations
open Mutils
open Putils

(* Not really a conversion but rather this: "T0_S1" -> "01" *)

let rec string_to_int s =
  try
    ignore (extfind s "accept_all");
    "666"  (* 666 is final state *)
  with _ ->
    try
      ignore (extfind s "accept_init");
      "0"  (* 666 is final state *)
    with _ ->
      if (String.length s >= 1) then (
	if not(isint s.[0]) then 
	  string_to_int (rmchar s s.[0])
	else
	  (string_of_char (s.[0])) ^ (string_to_int (lchop s))
      )
      else ""

 let last_state = ref ""
%}

%token NL REMOPEN REMCLOSE NEVER IF FI GOTO SKIP COLON SEMICOLON DCOLON
%token LBRACK RBRACK RARROW
%token <string> LABEL
%token <string> ID
%token <int> NUM

%start input
%type <unit> input

%% /* Grammar rules and actions follow */

input:      /* empty */ { }
          | input never { }
          ;

  never:    NEVER claim { } ;

  claim:
            LBRACK statement_list RBRACK { } ;

  statement_list:    /* empty */ { }
                  | statement_list statement { }
                  ;

  statement:
		  | state      { }
		  | iff        { }
		  | transition { }
		  | skip       { }
                  ;

  skip:     SKIP 
            { 
	      Declarations.transitions := !Declarations.transitions @
		["666", "(1)", "666"]
	    } ;

  iff:      IF           { }
          | FI SEMICOLON { }
          ;

  state:    ID COLON 
            { 
	      last_state := $1;
	      try 
		ignore (extfind $1 "accept");
		Declarations.states := !Declarations.states @ 
		  [(string_to_int $1)]
	      with Invalid_string -> Printf.printf ("") 
	    };
  

  transition:
            DCOLON LABEL RARROW GOTO ID
            { 
	      try
		ignore (extfind (rmchar $2 ' ') "(1)");
		Declarations.transitions := !Declarations.transitions @  
			[(string_to_int !last_state), 
			 "(1)", 
			 (string_to_int $5)]
	      with _ ->
		let labels = ref (break_label $2) in
		  while (!labels != []) do
		    let h = List.hd !labels in
		      Declarations.transitions := !Declarations.transitions @  
			[(string_to_int !last_state), 
			 (rmchar h ' '), 
			 (string_to_int $5)] ;
		      labels := List.tl !labels
		  done
	    };
  
%%

