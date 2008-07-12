(* 
   ltl2mon - converts an LTL formula into a FSM
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

(* file: nerverl.mll *)
{
  open Neverp (* Assumes the parser file is "neverp.mly". *)
}

let digit = ['0'-'9']
let id =  ['a'-'z' 'A'-'Z' '0'-'9' '_' '!']+
let label = ['('] ['!' '[' ']' '<' '>' '|' '&' '-' '>' ' ' ')' '(' '_' 'a'-'z' 'A'-'Z' '0'-'9']+ [')']

rule token = parse
  | [' ' '\t' '\n']	(* eat up whitespace *)
  	        { token lexbuf }
  | '{'         { LBRACK }
  | '}'         { RBRACK }
  | "->"        { RARROW }
  | "::"        { DCOLON }
  | ':'         { COLON }
  | ';'         { SEMICOLON }
  | '\n'	{ NL }
  | "/*"        { REMOPEN }
  | "*/"        { REMCLOSE }
  | "never"     { NEVER }
  | "if"        { IF }
  | "fi"        { FI }
  | "goto"      { GOTO }
  | "skip"      { SKIP }
  | label as label { LABEL (label) }
  | id as id    { ID (id) }
  | digit+ as num
                { NUM (int_of_string num) }
  | _ as c      { (* print_char c; *) token lexbuf }
  | eof		{ raise End_of_file }
