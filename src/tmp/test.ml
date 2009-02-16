let string_of_char = String.make 1

exception Invalid_string

let lchop s =
  if s = "" then "" else String.sub s 1 (String.length s - 1)

let rmchar (s : string) (c : char) =
  let new_s = ref "" in
  let slength = ref (String.length s - 1) in
    for i = 0 to !slength do
      if s.[i] != c then
	new_s := !new_s ^ (string_of_char s.[i])
    done;
    !new_s

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

let strip_action l =
  let sss = rmchar l ' ' in
  let ss = rmchar sss '(' in
  let s =  rmchar ss ')' in
    rmchar s '!'

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
	    
let ui = sigma_filter ["!m"] ["pm"; "mt"; "mt&&pm"; "m"; "m&&pm"; "m&&mt"; "m&&mt&&pm"; "<empty>"];;
