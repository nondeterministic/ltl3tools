open Dot
open Putils
open Mutils
open String
open Printf

let glob_arguments = ref []
and arg_function = ref "" 
and arg_return = ref "" 
and arg_args = ref "" 
and arg_arglist = ref ""
and cur_state = ref "(0,0)"

let is_valid_action action =
  not (String.contains action '&' || String.contains action '>')

let get_first_comp state =
  let state = rmchar state '-' in
  let index = String.index state ',' in
    String.sub state 0 index
      
let get_second_comp state =
  let state = rmchar state '-' in
  let index = String.index state ',' in
    String.sub state (index + 1) (String.length state - index - 1)

let encode_state state = 
  let state = Putils.strip_action state in  
  let state = rmchar state '\"' in
    if (safe_extfind state ",-" > 0) then
      "S" ^ (get_first_comp state) ^ "M"
    else if (safe_extfind state "-" < 0) then
      "S" ^ (get_first_comp state) ^ (get_second_comp state)
    else
      "SM" ^ (get_second_comp state)
	
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

let show_state_semantics state =
  if (safe_extfind state "SM" >= 0) then
    "Property violated"
  else if (String.contains state 'M') then
    "Property satisfied"
  else
    "Inconclusive"

let rec code_gen trans =
  match trans with
      [] -> Printf.printf ("");
    | (s,a,d)::t -> 
	(* If action is not "<empty>" or consists of multiple
	   propositions, go on.  (Multiple propositions make no sense
	   in our scenario.)  *)
	if (is_valid_action a) then
 	  begin
	    (* New state *)
	    if (s <> !cur_state) then
	      begin
		cur_state := s;
		Printf.printf ("\t\t\tbreak;\n");
		Printf.printf ("\t\tcase %s:\n") (encode_state !cur_state);
		Printf.printf ("\t\t\tprintf(\"Verdict: %s.\\n\");\n")
		  (show_state_semantics (encode_state !cur_state));
		Printf.printf ("\t\t\tif (input == %s)\n")
		  (String.uppercase (Putils.strip_action a));
		Printf.printf ("\t\t\t\tstate = %s;\n") (encode_state d);
	      end
		(* Old state: just add transition *)
	    else
	      begin
		Printf.printf ("\t\t\tif (input == %s)\n")
		  (String.uppercase (Putils.strip_action a));
		Printf.printf ("\t\t\t\tstate = %s;\n") (encode_state d);
	      end
	  end;
	code_gen t

let get_arg s =
  let l = String.index s '{' and r = String.index s '}' in
    String.sub s (l+1) (r-l-1)

let rec parse_arguments2 args =
  match args with
      [] ->
	glob_arguments := 
	  (!arg_function, !arg_return, !arg_args, !arg_arglist) :: 
	    !glob_arguments; 
	!glob_arguments
    | l::rest -> 
	if (safe_extfind l "FUNCTION" >= 0) then
	  begin
	    if (String.length !arg_function > 0) then
	      glob_arguments := 
		(!arg_function, !arg_return, !arg_args, !arg_arglist) :: 
		  !glob_arguments;
	    arg_function := get_arg l
	  end
	else if (safe_extfind l "RETURN" >= 0) then
	  arg_return := get_arg l
	else if (safe_extfind l "ARGS" >= 0) then
	  arg_args := get_arg l
	else if (safe_extfind l "ARG_LIST" >= 0) then
	  arg_arglist := get_arg l;
	parse_arguments2 rest

let parse_arguments args =
  parse_arguments2 args;
  !glob_arguments

let rec show_wrappers arg_list =
  match arg_list with
      [] -> Printf.printf ("\n")
    | (f,r,args,arglist)::t ->
	Printf.printf("%s I_WRAP_SONAME_FNNAME_ZU(NONE, %s)(%s)\n{\n")
	  r f arglist;
	Printf.printf("\t%s result;\n") r;
	Printf.printf("\tOrigFn fn;\n");
	Printf.printf("\tVALGRIND_GET_ORIG_FN(fn);\n");
	Printf.printf("\tfsm(%s);\n") (String.uppercase f);
	Printf.printf("\tCALL_FN_W_W(result, fn, %s);\n") args;
	Printf.printf("\treturn result;\n}\n\n");
	show_wrappers t

let rec show_defs_actions arg_list start =
  match arg_list with
      [] -> Printf.printf ("\n")
    | (f,_,_,_)::t ->
	Printf.printf("#define %s %i\n") (String.uppercase f) start;
	show_defs_actions t (start + 1)

let rec show_defs_states sts start =
  match sts with
      [] -> Printf.printf ("\n")
    | h::t ->
	Printf.printf("#define %s %i\n") (encode_state h)  start;
	show_defs_states t (start + 1)

let _ = 
  let dotfile = read_file Sys.argv.(1) in
  let (trans, sts) = Dot.parse dotfile in
  let arguments = parse_arguments (read_file Sys.argv.(2)) in
    Printf.printf ("#include <stdio.h>\n");
    Printf.printf ("#include <valgrind/valgrind.h>\n\n");
    show_defs_actions arguments 0;
    show_defs_states sts 0;
    Printf.printf ("int state;\n\n");
    Printf.printf ("void fsm (int input)\n{\n");
    Printf.printf ("\tswitch (state)\n\t{\n");
    Printf.printf ("\t\tcase S00:\n");
    code_gen trans;
    Printf.printf ("\t\t\tbreak;\n");
    Printf.printf ("\t}\n}\n\n");
    show_wrappers arguments;
