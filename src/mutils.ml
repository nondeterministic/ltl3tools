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

(* Mutils - more general helper functions *)

open String
open List

exception Invalid_string

(* if we have a list of lists, then unlist returns one straight list
   with all consecutive list elements *)

let rec unlist list =
  match list with
      h::t -> h @ (unlist t)
    | [] -> []

let extfind str sub =
  let sublen = ref (String.length sub) in
    if !sublen = 0 then
      0
    else
      let found = ref 0 in
      let len = ref (String.length str) in
        try
          for i = 0 to !len - !sublen do
            let j = ref 0 in
              while String.unsafe_get str (i + !j) = String.unsafe_get sub !j do
                incr j;
                if !j = !sublen then begin found := i; raise Exit; end;
              done;
          done;
          raise Invalid_string
        with
            Exit -> !found

let safe_extfind str sub =
  try
    extfind str sub
  with _ -> -1

let lchop s =
  if s = "" then "" else String.sub s 1 (String.length s - 1)

let string_of_char = String.make 1

let rmchar (s : string) (c : char) =
  let new_s = ref "" in
  let slength = ref (String.length s - 1) in
    for i = 0 to !slength do
      if s.[i] != c then
	new_s := !new_s ^ (string_of_char s.[i])
    done;
    !new_s

let isint c =
  let integers = "0123456789" in
    String.contains integers c

(* removes all double occurrences of an item in a list *)

let rec remove_doubles li =
match li with
    [] -> []
  | h::t -> if List.mem h t then 
      remove_doubles t
    else 
      h::(remove_doubles t)

(* if (0,1) and (1,0) are the same, then remove one of them in the
   list li, and return the remaining elements of li.  *)
        
let rec remove_double_pairs li =
  match li with
      [] -> []
    | (a,b)::t ->
        if List.exists (fun (x,y) -> if (a = y) && (b = x) then 
                          true
                        else 
                          false
                       ) t then
          remove_double_pairs t
        else
          (a,b) :: remove_double_pairs t

let rec unpair_list li =
  match li with
      [] -> []
    | (a,b)::t -> [a;b] @ unpair_list t

let rec zip l m =
  match l, m with
    | [], _ -> []
    | _, [] -> []
    | la::ls, ma::ms -> (la, ma) :: zip ls ms

(* Cartesian product of set xs and ys, represented as lists,
   respectively. *)

let rec cartesian xs ys =
  match xs, ys with
    | [], _ -> []
    | _, [] -> []
    | xs, ys -> 
        List.map (fun x -> (zip [x] ys)) xs @ cartesian xs (List.tl ys);;

let rec powerset =
  function
    | [] -> [[]]
    | h::t -> let p = powerset t in
        p @ List.map (fun x -> h::x) p

(* set1 - set2, where set1 and set2 are represented as lists *)

let rec diff set1 set2 = 
  match set1, set2 with
      [], s2 -> []
    | s1, [] -> s1
    | s1::ss1, s2 ->
        if List.mem s1 s2 then 
          diff ss1 s2
        else 
          s1 :: (diff ss1 s2) 

(* ["b"; "a"] -> ["a"; "b"] *)

let rec sort lst =
   match lst with
     [] -> []
   | head :: tail -> insert head (sort tail)
 and insert elt lst =
   match lst with
     [] -> [elt]
   | head :: tail -> 
       if elt <= head then
	 elt :: lst 
       else 
	 head :: insert elt tail
