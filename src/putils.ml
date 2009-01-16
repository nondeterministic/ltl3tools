(* 
   ltl2mon - converts an LTL formula into a FSM
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

(* Putils - parser helper functions *)

open String
open Mutils

(* 
   "(a) || (b && !c && d) || ... || (d)"
   ->  
   [ ["a"];  ["b && !c && d"]; ...; ["d"] ]
*)

let rec break_label label =
  let label = ref (rmchar label ' ') in
    if (String.length !label >= 1) then
      begin
        let action = ref "" in
          try
            while (String.length !label >= 1) do
              let c = !label.[0] in
                if (c != '(' && c != ')' && c != '|') then
                  action := !action ^ (string_of_char c)
                else if (c = ')') then
                  begin
                    label := lchop (!label);
                    raise Exit;
                  end;
                label := lchop (!label);
            done;
            !action :: break_label !label;
          with Exit -> !action :: break_label !label;
      end
    else []

(* # get_action_list "((a)&&!b&&c&&!d)";; *)
(* - : string list = ["a"; "!b"; "c"; "!d"] *)

 let rec get_action_list l =
   if (l = "()") then
     ["<empty>"]
   else (
     let sss = rmchar l ' ' in
     let ss = rmchar sss ')' in
     let s = ref (rmchar ss '(') in
       if (String.length !s >= 1) then (
         try
           let index = String.index !s '&' in
             (String.sub !s 0 index) ::
               get_action_list
               (String.sub !s (index + 2) (String.length !s - (index + 2)))
         with _ -> [!s]
       ) else [!s]
   )

(* removes (, ), !, and whitespaces from actions *)

let strip_action l =
  let sss = rmchar l ' ' in
  let ss = rmchar sss '(' in
  let s =  rmchar ss ')' in
    rmchar s '!'

(* For "a&&b" ["a"; "b"] is returned *)

let rec extract_propositions from_label =
  let l = ref (strip_action from_label) in
    try
      let upto = String.index !l '&' in
        (String.sub !l 0 (upto)) :: 
          (extract_propositions 
             (String.sub !l (upto + 2) ((String.length !l) - (upto + 2))))
    with _ -> [!l]

(* If action act contains proposition x return true otherwise false *)
(* Note that act may be "mb&&md" and x may be "m" or "md", etc. *)

let act_contains (act : string) (x : string) =
  let act = ref (extract_propositions act) in
    List.mem x !act

 (* Let action be a list of strings which together form an action,
    e.g., if action is ["!a"; "b"], then the actual action "!a&&b" is
    meant.  Sigma is an alphabet which is a list of strings (see also
    unfold_trans for a better description).  sigma_filter then returns
    those elements from the alphabet sigma, which "match" the action.
    E.g. ["!a"] would match all elements of sigma that do not contain
    an a, e.g., sigma_filter ["!a"] ["a"; "b"; "a&&b"; "<empty>"]
    would return ["b"; "<empty>"].  *)
     
let rec sigma_filter action sigma =
  match action with
      [] -> sigma
    | h::t ->
        if (h.[0] = '!') then (
          let sigmap =
            List.filter (fun s -> if not(act_contains s (lchop h)) then
                           true
                         else
                           false) sigma in
            sigma_filter t sigmap
        )
        else
          (
            let sigmap =
              List.filter (fun s -> if act_contains s h then
                             true
                           else
                             false) sigma in
              sigma_filter t sigmap
          )

(* [("0", "(!a && b)", "0")] returns ["a"; "b"; ... ]. *)

let rec extract_labels from_transitions =
  match from_transitions with
      [] -> []
    | (p,a,q)::t -> 
        (* Skip empty *)
        if ((safe_extfind a "empty") > 0) then
          (extract_labels t)
        else
          (extract_propositions a) @ (extract_labels t)

(* ["a"; "b"] -> "a&&b"
   or 
   ["a"] -> "a"
   Add brackets after by the calling function! *)

let rec convert_to_action proposition_list =
  match proposition_list with
      [] -> ""
    | h::t -> 
	if ((List.length proposition_list) = 1) then
	  h ^ (convert_to_action t)
	else (
	  let ands = 
	    if ((List.length t) >= 1) then 
	      "&&"
	    else 
	      "" in
	    h ^ ands ^ (convert_to_action t)
	)
	  
(* [ ["a"]; []; ["a"; "b"]; ["b"]] 
   ->  
   ["(a)"; "(<empty>)"; "(b)"; "(a&&b)"] *)

let rec actions_to_alphabet list_of_actions =
  List.map (fun action -> 
	      let a = convert_to_action action in
		if (a = "") then
		  "(<empty>)"
		else
		  "(" ^ a ^ ")") list_of_actions
    
let get_src_state (delta : string) =
  try
    let max = extfind delta "->" in
      rmchar (String.sub delta 0 max) '\"'
  with _ -> "get_src_state Error"

let get_dst_state (delta : string) =
  try
    let label = extfind delta "[label" in
    let deltap = ref (String.sub delta 0 (label)) in
    let from = ((extfind !deltap "->") + 2) in
      rmchar (String.sub !deltap from ((String.length !deltap - from))) '\"'
  with _ -> "get_dst_state Error"

let get_label (delta : string) =
  try
    let label = ref (extfind delta "[label=") in
    let deltap = ref
      (String.sub delta (!label + 8) (String.length delta - !label - 8)) in
    let max = extfind !deltap "\"]" in
      rmchar (String.sub !deltap 0 max) '\"'
  with _ -> "get_label Error"

let rec remove_universal_transitions trans =
  match trans with 
      [] -> []
    | h::t -> if (h = "1") then remove_universal_transitions t
      else h :: (remove_universal_transitions t)
