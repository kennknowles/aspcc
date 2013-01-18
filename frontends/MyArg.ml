
(* This is lifted and modified for prettiness from the ocaml stdlib *)

type key = string
type doc = string
type usage_msg = string
type anon_fun = (string -> unit)

type spec =
  | Unit of (unit -> unit)     (* Call the function with unit argument *)
  | Bool of (bool -> unit)     (* Call the function with a bool argument *)
  | Set of bool ref            (* Set the reference to true *)
  | Clear of bool ref          (* Set the reference to false *)
  | String of (string -> unit) (* Call the function with a string argument *)
  | Set_string of string ref   (* Set the reference to the string argument *)
  | Int of (int -> unit)       (* Call the function with an int argument *)
  | Set_int of int ref         (* Set the reference to the int argument *)
  | Float of (float -> unit)   (* Call the function with a float argument *)
  | Set_float of float ref     (* Set the reference to the float argument *)
  | Tuple of spec list         (* Take several arguments according to the
                                  spec list *)
  | Symbol of string list * (string -> unit)
                               (* Take one of the symbols as argument and
                                  call the function with the symbol. *)
  | Rest of (string -> unit)   (* Stop interpreting keywords and call the
                                  function with each remaining argument *)

exception Bad of string
exception Help of string

type error =
  | Unknown of string
  | Wrong of string * string * string  (* option, actual, expected *)
  | Missing of string
  | Message of string

exception Stop of error;; (* used internally *)

open Printf

let rec assoc3 x l =
  match l with
  | [] -> raise Not_found
  | (y1, y2, y3) :: t when y1 = x -> y2
  | _ :: t -> assoc3 x t
;;

let make_symlist prefix sep suffix l =
  match l with
  | [] -> "<none>"
  | h::t -> (List.fold_left (fun x y -> x ^ sep ^ y) (prefix ^ h) t) ^ suffix
;;

let pad str len =
	if (String.length str) > len then
		str
	else
		String.concat "" [str; String.make (len - (String.length str)) ' ']

let print_spec ?(pad_to = 0) buf (key, spec, doc) =
  match spec with
  | Symbol (l, _) -> bprintf buf "  %s %s %s\n" (pad key pad_to) (make_symlist "{" "|" "}" l)
                             doc
  | _ -> bprintf buf "  %s %s\n" (pad key pad_to) doc
;;

let rec maxkeylen ?(max = 0) li =
	match li with
	| [] -> max
	| (key, spec, doc) :: rest ->
		if (String.length key) > max then
			maxkeylen ~max:(String.length key) rest
		else
			maxkeylen ~max rest

let usage_b buf speclist errmsg =
  let maxlen = maxkeylen speclist in
  bprintf buf "%s\n" errmsg;
  List.iter (print_spec ~pad_to:maxlen buf) speclist;
  try ignore (assoc3 "-help" speclist)
  with Not_found -> bprintf buf "  %s Display this list of options\n" (pad "-help" maxlen);
  try ignore (assoc3 "--help" speclist)
  with Not_found -> bprintf buf "  %s Display this list of options\n" (pad "--help" maxlen);
;;

let usage speclist errmsg =
  let b = Buffer.create 200 in
  usage_b b speclist errmsg;
  eprintf "%s" (Buffer.contents b);
;;

let current = ref 0;;

let parse_argv ?(current=current) argv speclist anonfun errmsg =
  let l = Array.length argv in
  let b = Buffer.create 200 in
  let initpos = !current in
  let stop error =
    let progname = if initpos < l then argv.(initpos) else "(?)" in
    begin match error with
      | Unknown "-help" -> ()
      | Unknown "--help" -> ()
      | Unknown s ->
          bprintf b "%s: unknown option `%s'.\n" progname s
      | Missing s ->
          bprintf b "%s: option `%s' needs an argument.\n" progname s
      | Wrong (opt, arg, expected) ->
          bprintf b "%s: wrong argument `%s'; option `%s' expects %s.\n"
                  progname arg opt expected
      | Message s ->
          bprintf b "%s: %s.\n" progname s
    end;
    usage_b b speclist errmsg;
    if error = Unknown "-help" || error = Unknown "--help"
    then raise (Help (Buffer.contents b))
    else raise (Bad (Buffer.contents b))
  in
  incr current;
  while !current < l do
    let s = argv.(!current) in
    if String.length s >= 1 && String.get s 0 = '-' then begin
      let action =
        try assoc3 s speclist
        with Not_found -> stop (Unknown s)
      in
      begin try
        let rec treat_action = function
        | Unit f -> f ();
        | Bool f ->
            let arg = argv.(!current + 1) in
            begin try f (bool_of_string arg)
            with Invalid_argument "bool_of_string" ->
                   raise (Stop (Wrong (s, arg, "a boolean")))
            end;
            incr current;
        | Set r -> r := true;
        | Clear r -> r := false;
        | String f when !current + 1 < l ->
            f argv.(!current + 1);
            incr current;
        | Symbol (symb, f) when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            if List.mem arg symb then begin
              f argv.(!current + 1);
              incr current;
            end else begin
              raise (Stop (Wrong (s, arg, "one of: "
                                          ^ (make_symlist "" " " "" symb))))
            end
        | Set_string r when !current + 1 < l ->
            r := argv.(!current + 1);
            incr current;
        | Int f when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            begin try f (int_of_string arg)
            with Failure "int_of_string" ->
                   raise (Stop (Wrong (s, arg, "an integer")))
            end;
            incr current;
        | Set_int r when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            begin try r := (int_of_string arg)
            with Failure "int_of_string" ->
                   raise (Stop (Wrong (s, arg, "an integer")))
            end;
            incr current;
        | Float f when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            begin try f (float_of_string arg);
            with Failure "float_of_string" ->
                   raise (Stop (Wrong (s, arg, "a float")))
            end;
            incr current;
        | Set_float r when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            begin try r := (float_of_string arg);
            with Failure "float_of_string" ->
                   raise (Stop (Wrong (s, arg, "a float")))
            end;
            incr current;
        | Tuple specs ->
            List.iter treat_action specs;
        | Rest f ->
            while !current < l - 1 do
              f argv.(!current + 1);
              incr current;
            done;
        | _ -> raise (Stop (Missing s))
        in
        treat_action action
      with Bad m -> stop (Message m);
         | Stop e -> stop e;
      end;
      incr current;
    end else begin
      (try anonfun s with Bad m -> stop (Message m));
      incr current;
    end;
  done;
;;

let parse l f msg =
  try
    parse_argv Sys.argv l f msg;
  with
  | Bad msg -> eprintf "%s" msg; exit 2;
  | Help msg -> printf "%s" msg; exit 0;
;;
