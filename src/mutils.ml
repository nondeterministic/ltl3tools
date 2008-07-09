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
  let sublen = String.length sub in
    if sublen = 0 then
      0
    else
      let found = ref 0 in
      let len = String.length str in
        try
          for i = 0 to len - sublen do
            let j = ref 0 in
              while String.unsafe_get str (i + !j) = String.unsafe_get sub !j do
                incr j;
                if !j = sublen then begin found := i; raise Exit; end;
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

let rec rmchar s c =
  if (String.length s >= 1) then (
    if (String.get s 0 = c) then
      rmchar (lchop s) c
    else
      (string_of_char (String.get s 0)) ^ (rmchar (lchop s) c)
  )
  else ""

let isint c =
  let integers = "0123456789" in
    String.contains integers c

(* removes all double occurrences of an item in a list *)

let rec remove_doubles li =
  match li with
      [] -> []
    | h::t -> h :: (remove_doubles (List.filter ((<>) h) t))

let rec powerset =
  function
    | [] -> [[]]
    | h::t -> let p = powerset t in
        p @ List.map (fun x -> h::x) p

(* ["b"; "a"] -> ["a"; "b"] *)

let rec sort lst =
   match lst with
     [] -> []
   | head :: tail -> insert head (sort tail)
 and insert elt lst =
   match lst with
     [] -> [elt]
   | head :: tail -> if elt <= head then elt :: lst else head :: insert elt tail
