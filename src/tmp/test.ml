#use "mutils.ml";;

let lchop s =
  if s = "" then "" else String.sub s 1 (String.length s - 1)

let string_of_char = String.make 1

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

(* Returns ["a"; "b"; ... ] from a list of transitions. *)

let rec extract_labels from_transitions =
  match from_transitions with
      [] -> []
    | (p,a,q)::t -> 
	(* Skip empty *)
	if ((safe_extfind a "empty") > 0) then
	  (extract_labels t)
	else
	  (extract_propositions a) @ (extract_labels t)

(* ["b"; "a"] -> ["a"; "b"] *)

let rec sort lst =
   match lst with
     [] -> []
   | head :: tail -> insert head (sort tail)
 and insert elt lst =
   match lst with
     [] -> [elt]
   | head :: tail -> if elt <= head then elt :: lst else head :: insert elt tail
