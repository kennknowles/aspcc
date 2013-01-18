
(** The standard set of VbScript functions and constants *)

open VbValues
open VbClass
open VbTypes
open Runtime
open Netdate
open Printf

let invalid_arg_count name expected params =
    raise (Invalid_arg_count (name, expected, List.length params))

let not_implemented_yet = (fun _ -> raise (Not_implemented "builtin function"))

(** {4 VbScript constants/enumumerations} *)

(** {6 Colors} *)

type color =
	    VbBlack | VbRed | VbGreen | VbYellow | VbBlue | VbMagenta |
	        VbCyan | VbWhite

let color_of_int i =
	match i with
	    | 0x000000 -> VbBlack
	    | 0x0000ff -> VbRed
	    | 0x00ff00 -> VbGreen
	    | 0x00ffff -> VbYellow
	    | 0xff0000 -> VbBlue
	    | 0xff00ff -> VbMagenta
	    | 0xffff00 -> VbCyan
	    | 0xffffff -> VbWhite
	    | _ -> raise (Failure "Invalid color")

(** {6 Comparison} *)

type compare_mode =
	    VbTextCompare | VbBinaryCompare | VbDatabaseCompare

let compare_mode_of_int i =
	match i with
	    | 0 -> VbBinaryCompare
	    | 1 -> VbTextCompare
	    | 2 -> VbDatabaseCompare (** I don't even really know what this means *)
	    | _ -> raise (Failure "Invalid compare mode")

(** {6 Date stuff} *)

type date_format =
	    VbGeneralDate | VbLongDate | VbShortDate | VbLongTime | VbShortTime

let date_format_of_int i =
	match i with
	    | 0 -> VbGeneralDate
	    | 1 -> VbLongDate
	    | 2 -> VbShortDate
	    | 3 -> VbLongTime
	    | 4 -> VbShortTime
	    | _ -> raise (Failure "Invalid date format")

type day_of_week =
	    VbSunday | VbMonday | VbTuesday | VbWednesday | VbThursday | VbFriday |
	        VbSaturday

let day_of_week_of_int i =
	match i with
	    | 1 -> VbSunday
	    | 2 -> VbMonday
	    | 3 -> VbTuesday
	    | 4 -> VbWednesday
	    | 5 -> VbThursday
	    | 6 -> VbFriday
	    | 7 -> VbSaturday
	    | _ -> raise (Failure "Invalid day of week")

(** These are ignored in all my functions anyhow *)
type week_of_year =
	    VbFirstJan1 | VbFirstFourDays | VbFirstFullWeek

(** Technically I think there is supposed to be some treatment of 
  zero as a "system default" but I'm not dealing with system defaults *)
let week_of_year_of_int i =
	match i with
	    | 1l -> VbFirstJan1
	    | 2l -> VbFirstFourDays
	    | 3l -> VbFirstFullWeek
	    | _ -> raise (Failure "Invalid week of year")



(** {6 Date/Time Intervals} *)

type interval = 
	    Year | Quarter | Month | DayOfYear | Day | WeekDay | WeekOfYear |
	        Hour | Minute | Second
		            
let interval_of_string v =
	match String.lowercase v with
	    | "yyyy" -> Year
	    | "q" -> Quarter
	    | "m" -> Month
	    | "y" -> DayOfYear
	    | "d" -> Day
	    | "w" -> WeekDay
	    | "ww" -> WeekOfYear
	    | "h" -> Hour
	    | "n" -> Minute
	    | "s" -> Second
	    | _ -> raise (Failure "Invalid interval specification")


(** {6 Tristate Values} *)

type tristate = TristateTrue | TristateFalse | TristateUseDefault

let tristate_of_int i =
	match i with
	    | (-1) -> TristateTrue
	    | 0 -> TristateFalse
	    | (-2) -> TristateUseDefault
	    | _ -> raise (Failure "Invalid tristate")

let bool_of_tristate = 
    function
	    | TristateFalse -> false
	    | _ -> true

let get_tribool t = (bool_of_tristate (tristate_of_int (get_int t)))

(** {4 VbScript Builtin Functions} *)

(* TODO: make the error messages from exceptions more interesting *)
let asp__abs params =
    let x = arg1 "Abs" params in
    ref (num_apply Int32.abs abs_float x)

let asp__array params =
	ref (Array (Single (Array.of_list
		                    (List.map (fun x -> ref !x) params))))

let asp__asc params =
    let x = arg1 "Asc" params in
    wrap_int (Char.code (get_string !x).[0])
        
let asp__atn params =
    let x = arg1 "Atn" params in
    ref (Float (atan (get_float !x)))

let asp__cbool params =
    let x = arg1 "CBool" params in
	ref (Bool (get_bool !x))

(** difference from MS: converts to int *)
let asp__cbyte params =
    let x = arg1 "CByte" params in
	wrap_byte (get_int !x)

(** difference from MS: converts to float *)
let asp__ccur params =
    let x = arg1 "CByte" params in
	ref (Float (get_float !x))

(** difference from MS: converts to string *)	
let asp__cdate params =
	match params with
	    | [x] -> ref (DateTime (get_date !x))
	    | _ -> invalid_arg_count "CDate" 1 params
              
(** difference from MS: converts to float *)	
let asp__cdbl params =
	match params with
	    | [x] -> 
              (match !x with
                   | DateTime (t, d) -> wrap_float (comfloat_of_netdate d)
                   | _ -> wrap_float (get_float !x))
                    
	    | _ -> invalid_arg_count "CDbl" 1 params

let asp__chr params =
	match params with
	    | [x] -> wrap_string (String.make 1 (Char.chr (get_int !x)))
	    | _ -> invalid_arg_count "Chr" 1 params

(** difference from MS: converts to int *)	
(* TODO: this breaks for floats, because get_int does
   not ever round, etc *)
let asp__cint params =
	match params with
	    | [x] -> wrap_int32 (get_int32 !x)
	    | _ -> invalid_arg_count "CInt" 1 params

(** difference from MS: converts to int *)	
(* TODO: make sure this rounds rather than truncating, like 'fix' or 'int' *)
let asp__clng params =
	match params with
	    | [x] -> wrap_int32 (get_int32 !x)
	    | _ -> invalid_arg_count "CLng" 1 params

let asp__cos params =
	match params with
	    | [x] -> ref (Float (cos (get_float !x)))
	    | _ -> invalid_arg_count "Cos" 1 params

let asp__create_object runtime params =
    let classname = Symbol.of_string (get_string !(arg1 "CreateObject" params)) in
    wrap_object ((get_class runtime classname) ())

(** difference from MS: converts to float *)
let asp__csng params =
	match params with
	    | [x] -> ref (Float (get_float !x))
	    | _ -> invalid_arg_count "CSng" 1 params

let asp__cstr params =
	match params with
	    | [x] -> ref (String (get_string !x))
	    | _ -> invalid_arg_count "CStr" 1 params

let asp__date params =
	match params with
	    | [] -> wrap_date (Both, netdate_of_unixfloat (current_unixfloat ()))
	    | _ -> invalid_arg_count "Date" 0 params


let asp__date_add params =
	match params with
	    | [interval; number; date] ->
		      let n, d = get_int !number, get_date !date in
		      (* TODO: make sure netstring handles this right when it finally
		         gets formatted; otherwise hax netstring *)
		      ref (DateTime  (  (match d with
						             | (t, _) -> t), 
						        (match d with
					 	             | (_, d) ->	 
		                                   (match interval_of_string (get_string !interval) with
		                                        | Year -> {d with year = d.year + n}
		                                        | Quarter -> {d with month = d.month + n*3}
		                                        | Month -> {d with month = d.month + n}
		                                        | DayOfYear | Day | WeekDay -> {d with day = d.day + n}
		                                        | WeekOfYear -> {d with day = d.day + n*7}
		                                        | Hour -> {d with hour = d.hour + n}
		                                        | Minute -> {d with minute = d.minute + n}
		                                        | Second -> {d with second = d.second + n}
		                                   ))))

	    | _ -> raise (Invalid_arg_list "DateAdd")

let asp__date_serial params =
	match params with
	        (* TODO: I don't know how netstring handles it if everything is
		       correct except the week_day... that would be much faster, but	
		       for now I'll just make something it can parse *)
	    | [year; month; day] ->
		      ref (DateTime (Date,
			                 (Netdate.parse (Printf.sprintf "%i/%i/%i" 
				                                 (get_int !month)
				                                 (get_int !day)
				                                 (get_int !year)
			                                ))))
              (*		ref (Date {
			            year = !year;
			            month = !month;
			            day = !day;
			            hour = 0;
			            minute = 0;
			            second = 0;
			            zone = localzone;
			            week_day = 0 *)
	    | _ -> raise (Invalid_arg_list "DateSerial")

(** difference from MS: ignores firstdayofweek and firstdayofyear for now *)
(* TODO: use Unix dates instead of netstring dates, because they have
   more info.  All we have to be sure and do is to convert the timezone
   when getting  the string for the date, or when formatting it.  Use
   netdate only for parsing *)
(*let asp__date_part params =
  match params with
  | [interval; date]
  | [interval; date; _]
  | [interval; date; _; _] ->
  let d = get_date !date in
  (match interval_of_string (get_string !interval) with
  | Year -> d.year
  | Quarter -> (d.month - 1) / 3 + 1
  | Month -> d.month

  | _ -> raise (Invalid_arg_list "DatePart")
*)

let asp__exp params =	
	match params with
	    | [x] -> ref (Float (exp (get_float !x)))
	    | _ -> raise (Invalid_arg_list "Exp")


let asp__filter params =
	let inputstrings, value, incl, comp =
		(match params with
		     | [inputstrings; value] -> 
			       (get_array !inputstrings), (get_string !value),
			       true, VbBinaryCompare 

		     | [inputstrings; value; incl] ->
			       (get_array !inputstrings), (get_string !value),
			       (get_bool !incl), VbBinaryCompare


		     | [inputstrings; value; incl; comparemode] ->
			       (get_array !inputstrings), (get_string !value),
			       (get_bool !incl), (compare_mode_of_int (get_int !comparemode))

		     | _ -> raise (Invalid_arg_list "Filter"))
	in

	let regex = ( match comp with
					  | VbTextCompare -> Str.regexp_case_fold value
					  | _ -> Str.regexp value)
	in

	let pred = 
		function candidate ->
			try	ignore (Str.search_forward regex (get_string !candidate) 0);
				incl
			with Not_found -> not incl
	in	

	match inputstrings with
	    | Single arr ->
		      ref (Array (Single (Array.of_list
				                      (List.filter pred (Array.to_list arr)))))

	    | _ -> raise (Invalid_argument "Filter")

let asp__fix params =
	match params with
	    | [x] -> ( 
		      match !x with
		          | Null -> ref Null
		          | Int i -> ref (Int i)

		          (* Note: Fix truncates, while Int floors *)
		          | z -> wrap_int32 (Int32.of_float (get_float z))
		  )
	    | _ -> raise (Invalid_arg_list "Fix")

(** difference from MS: defaults to 2 decimals, rather than local settings *)
(** difference from MS: also, use my conception of what defaults should be,
  ignoring tristate's default *)
let asp__format_currency params =
	let expr, decimals, lead_digit, negparens, groupdigits =
		match params with
		    | [e] -> (get_float !e), 2, true, false, true
		    | [e;d] -> (get_float !e), (get_int !d), true, false, true
		    | [e;d;l] -> (get_float !e), (get_int !d), 
			  (get_tribool !l), false, true
		    | [e;d;l;n] ->
			      (get_float !e), (get_int !d), 
			      (bool_of_tristate (tristate_of_int (get_int !l))), 
			      (bool_of_tristate (tristate_of_int (get_int !n))),
			      true
		    | [e;d;l;n;g] ->
			      (get_float !e), (get_int !d), 
			      (bool_of_tristate (tristate_of_int (get_int !l))), 
			      (bool_of_tristate (tristate_of_int (get_int !n))), 
			      (bool_of_tristate (tristate_of_int (get_int !g)))

		    | _ -> raise (Invalid_arg_list "FormatCurrency")

	in

	ref (String "format_currency not finished")


(** difference from MS, the vbGeneralDate displays both parts, no matter
  which are present... because we don't have that concept *)
let asp__format_date_time params =
	let date, form =
		match params with
		    | [d] -> (get_date !d), VbGeneralDate
		    | [d;f] -> (get_date !d), (date_format_of_int (get_int !f))
		    | _ -> raise (Invalid_arg_list "FormatDateTime")
	in

	(* TODO: create the pattern with a match, but just output it once *)
	match form with
	    | VbGeneralDate -> (match date with
		                        | (Both, d) -> ref (String (Netdate.format "%D %T" d))
		                        | (Date, d) -> ref (String (Netdate.format "%D" d))
		                        | (Time, d) -> ref (String (Netdate.format "%T" d)))
	    | VbLongDate -> (match date with
			                     (** FIXME I need %A to match properly, but I get
			                       an exception thrown if I use it. This is incorrect for now.
			                       Throws exception on 
			                       var = CDate("1/1/1900 12:45 am")
			                       response.write FormatDateTime(var, 1) & " vbLongDate<br>" & vbNewLine

			                       But not on:
		                           response.write FormatDateTime(date(), 1) & vbNewLine 

			                       Will take a look later and fix this line, or dump when we dump Netdate entirely.
			                     *)
		                     | (Both, d) -> ref (String (Netdate.format "WEEKDAY_NAME, %B %d, %Y" d))
	                               (*	| (Both, d) -> ref (String (Netdate.format "%A, %B %d, %Y" d)) *)
		                     | (Date, d) -> ref (String (Netdate.format "%A, %B %d, %Y" d))
		                     | (Time, d) -> ref (String (Netdate.format "%A, %B %d, %Y" d))) 
	    | VbShortDate -> (match date with
		                      | (Both, d) -> ref (String (Netdate.format "%D" d))
		                      | (Date, d) -> ref (String (Netdate.format "%D" d))
		                      | (Time, d) -> ref (String (Netdate.format "%D" d)))
	    | VbLongTime -> (match date with
		                     | (Both, d) -> ref (String (Netdate.format "%T" d))
		                     | (Date, d) -> ref (String (Netdate.format "%T" d))
		                     | (Time, d) -> ref (String (Netdate.format "%T" d)))
	    | VbShortTime -> (match date with
		                      | (Both, d) -> ref (String (Netdate.format "%H:%M" d))
		                      | (Date, d) -> ref (String (Netdate.format "%H:%M" d))
		                      | (Time, d) -> ref (String (Netdate.format "%H:%M" d)))


(** difference from MS: use my defaults :) rather than regional settings *)
let asp__format_number params =
	let expr, decimals, lead, parens, group =
		(match params with
		     | [e] -> !e, 2, true, false, true
		     | [e;d] -> !e, (get_int !d), true, false, true
		     | [e;d;l] -> !e, (get_int !d), 
			   (get_tribool !l), false, true
		     | [e;d;l;p] -> !e, (get_int !d), (get_tribool !l),
			   (get_tribool !p), true
		     | [e;d;l;p;g] -> !e, (get_int !d), 
			   (get_tribool !l), (get_tribool !p), (get_tribool !g)
		     | _ -> raise (Invalid_arg_list "FormatNumber"))
	in

    wrap_string (sprintf "%.*f" decimals (get_float expr))
        
let asp__format_percent params =
	let expr, decimals, lead, parens, group =
		(match params with
		     | [e] -> !e, 2, true, false, true
		     | [e;d] -> !e, (get_int !d), true, false, true
		     | [e;d;l] -> !e, (get_int !d), (get_tribool !l), false, true
		     | [e;d;l;p] -> !e, (get_int !d), (get_tribool !l),
			   (get_tribool !p), true
		     | [e;d;l;p;g] -> !e, (get_int !d), 
			   (get_tribool !l), (get_tribool !p), (get_tribool !g)
		     | _ -> raise (Invalid_arg_list "FormatPercent"))
	in

	ref (String "FormatPercent not completed")

let asp__hex params =
	match params with
	    | [x] -> (
		      match !x with
		          | Null -> ref Null
		          | Empty -> ref (Int 0l)
		          | _ -> ref (String (Printf.sprintf "%lX" (get_int32 !x))) )
	    | _ -> raise (Invalid_arg_list "Hex")

(** godamn MS starting strings from 1 *)
(* TODO: add proper handling of null values *)
let asp__in_str params =
	let start, haystack, needle, compare = (
		match params with	
		    | [h;n] -> 1, (get_string !h), (get_string !n), VbBinaryCompare
		    | [s;h;n] -> (get_int !s), (get_string !h), (get_string !n),
			  VbBinaryCompare
		    | [s;h;n;c] -> (get_int !s), (get_string !h), (get_string !n),
			  (compare_mode_of_int (get_int !c))
		    | _ -> raise (Invalid_arg_list "InStr")
	)
	in
	(* TODO: make sure these are escaped, so the regex isn't accidentally
	   fancier than a literal match *)
	let regex = (match compare with
				     | VbBinaryCompare -> Str.regexp needle
				     | VbDatabaseCompare | VbTextCompare -> Str.regexp_case_fold needle)
	in
	try
		wrap_int ((Str.search_forward regex haystack (start - 1)) + 1)
	with
		| Not_found -> ref (Int 0l)

let asp__in_str_rev params =
	let start, haystack, needle, compare = (
		match params with	
		    | [h;n] -> 1, (get_string !h), (get_string !n), VbBinaryCompare
		    | [h;n;s] -> (get_int !s), (get_string !h), (get_string !n),
			  VbBinaryCompare
		    | [h;n;s;c] -> (get_int !s), (get_string !h), (get_string !n),
			  (compare_mode_of_int (get_int !c))
		    | _ -> raise (Invalid_arg_list "InStr")
	)
	in
	(* TODO: make sure these are escaped, so the regex isn't accidentally
	   fancier than a literal match *)
	let regex = (match compare with
				     | VbBinaryCompare -> Str.regexp needle
				     | VbDatabaseCompare | VbTextCompare -> Str.regexp_case_fold needle)
	in
	try
		wrap_int ((Str.search_backward regex haystack (start - 1)) + 1)
	with
		    Not_found -> ref (Int 0l)

let asp__int params =
	match params with
	    | [x] -> ref (Int (Int32.of_float (get_float !x)))
	    | _ -> raise (Invalid_arg_list "Int")

let asp__is_array params =
	match params with
	    | [x] -> (match !x with 
				      | Array _ -> ref (Bool true)
				      | _ -> ref (Bool false))
	    | _ -> raise (Invalid_arg_list "IsArray")

let asp__is_date params =
	match params with 
	    | [x] -> ( try ignore(parse_epoch (get_string !x)); ref (Bool true) with
				           _ -> ref (Bool false))
	    | z -> raise (Invalid_arg_count ("IsDate", 1, List.length z))
			  

let asp__is_empty params =
	match params with
	    | [x] -> (match !x with 
				      | Empty -> ref (Bool true)
				      | _ -> ref (Bool false))
	    | _ -> raise (Invalid_arg_list "IsEmpty")

let asp__is_null params =
	match params with
	    | [x] -> (match !x with 
				      | Null -> ref (Bool true)
				      | _ -> ref (Bool false))
	    | _ -> raise (Invalid_arg_list "IsNull")

let asp__is_numeric params =
	match params with
	    | [x] -> (	try ignore (get_float !x); ref (Bool true)
				    with Cannot_convert _ | Failure _ -> ref (Bool false))
	    | _ -> raise (Invalid_arg_list "IsNumeric")

let asp__is_object params =
	match params with
	    | [x] -> (match !x with 
				      | Object _ -> ref (Bool true)
				      | _ -> ref (Bool false))
	    | _ -> raise (Invalid_arg_list "IsObject")

let asp__join params =
	let arr, glue = (
		match params with
		    | [a] -> (get_array !a), ""
		    | [a;g] -> (get_array !a), (get_string !g)
		    | _ -> raise (Invalid_argument "Join")
	)
	in
	match arr with 
	    | Single x ->
		      ref (String (
		               String.concat glue (List.map 
								               (fun y -> get_string !y)
								               (Array.to_list x))
		           ))

	    | Multi y -> raise (Invalid_argument "Cannot call join on multidimensional array")


(** This just unboxes to check if it is an array, then returns zero *)
let asp__lbound params =
	match params with
	    | [x] -> (match !x with 
				      | Array _ -> ref (Int 0l)
				      | _ -> raise (Invalid_arg_list "LBound"))
	    | _ -> raise (Invalid_arg_list "LBound")

let asp__lcase params =
	match params with
	    | [x] -> ref (String (String.lowercase (get_string !x)))
	    | _ -> raise (Invalid_arg_list "LBound")

let asp__left params =
	match params with
	    | [str; len] -> 
              let s, l = get_string !str, get_int !len in
              if String.length s < l then 
                  wrap_string s
              else
                  wrap_string (Str.first_chars s l)

	    | _ -> raise (Invalid_arg_list "LBound")

let asp__len params =
	match params with
	    | [x] -> wrap_int (String.length (get_string !x))
	    | _ -> raise (Invalid_arg_list "Len")

let asp__ltrim params =
	match params with
	    | [x] -> ref (String (
		                  Str.replace_first (Str.regexp "^[ \t\n]") "" (get_string !x)
		              ))
	    | _ -> raise (Invalid_arg_list "Len")

let asp__log params =
	match params with
	    | [x] -> ref (Float (log (get_float !x)))
	    | _ -> raise (Invalid_arg_list "Log")

(* note: the -1 on start is a correction for MS indexing starting from 1 *)
let asp__mid params =
	let str,start,len = (
		match params with
		    | [s;st] -> (get_string !s), (get_int !st) - 1, -1 
		    | [s;st;l] -> (get_string !s), (get_int !st) - 1, (get_int !l)
		    | _ -> raise (Invalid_arg_list "Mid")
	)
	in
	let len = (if len = -1 then (String.length str) - start else len) in

	ref (String (
		     String.sub str start len
	     ))

let asp__now = asp__date

let asp__oct params =
	match params with
	    | [x] -> (
		      match !x with
		          | Null -> ref Null
		          | Empty -> ref (Int 0l)
		          | _ -> ref (String (Printf.sprintf "%lo" (get_int32 !x))) )
	    | _ -> raise (Invalid_arg_list "Hex")

let asp__right params =
	match params with
	    | [str; len] ->
              let s, l = get_string !str, get_int !len in
              if String.length s < l then 
                  wrap_string s 
              else
		          wrap_string (Str.last_chars s l)

	    | _ -> raise (Invalid_arg_list "Right")

let asp__rnd_state = ref (Random.get_state ());;
let asp__rnd params =
	match params with
	    | [] -> asp__rnd_state := Random.get_state ();
			  ref (Float (Random.float 1.0))

	    | [x] -> (
		      let i = get_int !x in
		      if i < 0 then
			      (Random.init i; asp__rnd_state := Random.get_state ();
			       ref (Float (Random.float 1.0)))
		      else (

			      (* TODO: figure out why tthis isn't working... it works in the
			         toplevel *)
			      if i = 0 then
				      (Random.set_state !asp__rnd_state;
				       ref (Float (Random.float 1.0)))
			      else
				      (asp__rnd_state := Random.get_state (); 
				       ref (Float (Random.float 1.0)))
	          ))
	    | _ -> raise (Invalid_arg_list "Rnd")

let asp__sin params =
	match params with
	    | [x] -> ref (Float (sin (get_float !x)))
	    | _ -> raise (Invalid_arg_list "Sin")

let asp__space params =
	match params with
	    | [x] -> ref (String (String.make (get_int !x) ' '))
	    | _ -> raise (Invalid_arg_list "Space")

let asp__split params =
    let value, pattern, count, compare =
        match params with
            | [v] -> get_string !v, " ", -1, VbBinaryCompare 
            | [v;p] -> get_string !v, get_string !p, -1, VbBinaryCompare
            | [v;p;c] -> get_string !v, get_string !p, get_int !c, VbBinaryCompare
            | [v;p;c;co] -> get_string !v, get_string !p, get_int !c, compare_mode_of_int (get_int !co)
            | li -> raise (Invalid_arg_count ("Split", 1, List.length li))
    in

    let reg = match compare with
        | VbBinaryCompare -> Str.regexp_string pattern
        | VbDatabaseCompare 
        | VbTextCompare -> Str.regexp_string_case_fold pattern
    in
    
    wrap_array (Array.of_list (List.map wrap_string (Str.split reg value)))

let asp__str_comp params =
    let s1, s2, comp =
        match params with
            | [s1;s2] -> !s1, !s2, VbBinaryCompare
            | [s1;s2;c] -> !s1, !s2, compare_mode_of_int (get_int !c)
            | z -> raise (Invalid_arg_count ("StrComp", 2, List.length z))
    in
    
    match comp with
        | VbBinaryCompare -> wrap_int (compare (get_string s1) (get_string s2))
        | VbTextCompare
        | VbDatabaseCompare -> wrap_int (compare 
                                             (String.lowercase (get_string s1))
                                             (String.lowercase (get_string s2)))
              
let asp__string params =
    let number, character = arg2 "String" params in
    wrap_string (String.make (get_int !number) (get_string !character).[0])

let asp__tan params =
	match params with
	    | [x] -> ref (Float (tan (get_float !x)))
	    | _ -> raise (Invalid_arg_list "Tan")

let asp__timer params =
    arg0 "Timer" params;
    wrap_float (current_unixfloat ())

let asp__trim params =
    (* not amazingly efficient, but I'm better at recursion than regexps *)
    let s = get_string !(arg1 "Trim" params) in
    try
        let lastindex = (String.length s) - 1 in
        let start_index = Str.search_forward (Str.regexp "[^ \t\n]") s 0 in
        let end_index = Str.search_backward (Str.regexp "[^ \t\n]") s lastindex in
	    wrap_string (String.sub s start_index (end_index - start_index + 1))
    with
        | Not_found -> wrap_string ""

let asp__typename params =
    match !(arg1 "TypeName" params) with
        | Empty -> wrap_string "Empty"
        | Null -> wrap_string "Null"
        | Int _ -> wrap_string "Int"
        | String _ -> wrap_string "String"
        | Object None -> wrap_string "Nothing"
        | Object (Some o) -> wrap_string (Symbol.to_string !o # classname)
              

let asp__ubound params =
    let arr, dim = match params with
        | [x] -> get_array !x, 0
        | [x;y] -> get_array !x, (get_int !y) - 1
        | z -> raise (Invalid_arg_count ("UBound", 2, List.length z))
    in

    wrap_int (array_length arr dim)
    

let asp__vartype params =
    let var = arg1 "VarType" params in
    wrap_int
        (match !var with
            | Empty -> 0
            | Null -> 1
            | Int _ -> 3 (* Long *)
            | String _ -> 8
            | Bool _ -> 11
            | Object _ -> 9
            | Array _ -> 8192
            | DateTime _ -> 7
            | Float _ -> 5 (* Dbl *) )
                  
class vbscript_err =
object(self)
	inherit opaque_object
	    
	val mutable description = ""
	val mutable number = 0
	val mutable source = ""

	(** Error handler - this is what should be registered with the runtime *)
    (*	method error_handler err =
	(*
		match err with
		|	insert microsoft's lame error categories here, assign source, number, description,
		and possibly even more fields
		| *)
		() *)

    method strname = "Error"

	(** Description *)
	method private description_let params =
		let v = arg1 "Err.Description Let" params in
		description <- get_string !v;
		ref Null
	        
	(** Number *)
	method private number_get params =
		arg0 "Err.Number Get" params;
		wrap_int number

	method private number_let params =
		let v = arg1 "Err.Number Let" params in
		number <- get_int !v;
		ref Null
	        
	(** Source *)
	method private source_get params =
		arg0 "Err.Source Get" params;
		ref (String source)

	method private source_let params =
		let v = arg1 "Err.Source Let" params in
		source <- get_string !v;
		ref Null
	        
	(** Clear *)
	method private clear_method params =
		arg0 "Err.Clear Method" params;
		description <- "";
		number <- 0;
		source <- "";
		ref Null

	(** Raise *)
	method private raise_method params =
		(match params with
		     | [number] -> () (* error_handler new err *)
		     | [number; source] -> ()
		     | [number; source; description] -> ()
		     | [number; source; description; helpfile] -> ()
		     | [number; source; description; helpfile; helpcontext] -> ()
		     | _ -> ());
		ref Null

    method m_gets name params =
        match name with
            | "description" ->
		          arg0 "Err.Description Get" params;
		          ref (String description)

		    | "helpcontext" -> not_implemented_yet params
		    | "helpfile" -> not_implemented_yet params
		    | "number" -> self#number_get params
		    | "source" -> self#source_get params
		          
		    (* Methods *)
            | "clear" -> self#clear_method params
		    | "raise" -> self#raise_method params
		          
		    | _ -> self#not_found name params
                  
	method m_lets name params =
	    match name with
		    (* Properties *) 
		    | "description" -> self#description_let params
		    | "helpcontext" -> not_implemented_yet params
		    | "helpfile" -> not_implemented_yet params
		    | "number" -> self#number_let params
		    | "source" -> self#source_let params


		    | _ -> self#not_found name params

end






let load runtime =
	List.iter 
		(fun (name,func) -> Runtime.add_function runtime (Symbol.of_string name) func)
		[
		    (*=============== Builtin functions =============*)
			"Abs", asp__abs;
			"Array", asp__array;
			"Asc",  asp__asc;
			"Atn", asp__atn;
			"CBool", asp__cbool;
			"CByte", asp__cbyte;
			"CCur", asp__ccur;
			"CDate", asp__cdate;
			"CDbl", asp__cdbl;
			"Chr", asp__chr;
			"CInt", asp__cint;
			"CLng", asp__clng;
			"Cos", asp__cos;
			"CreateObject", asp__create_object runtime;
			"CSng", asp__csng;
			"CStr", asp__cstr;
			"Date", asp__date;
			"DateAdd", asp__date_add;
			"DateDiff", not_implemented_yet;
			"DatePart", not_implemented_yet;
			"DateSerial", asp__date_serial;
			"DateValue", not_implemented_yet;
			"Day", not_implemented_yet;
			"Escape", not_implemented_yet;
			(* "Eval" - built into runtime  *)
			"Exp", asp__exp;
			"Filter", asp__filter;
			"Fix", asp__fix;
			"FormatCurrency", not_implemented_yet;
			"FormatDateTime", asp__format_date_time;
			"FormatNumber", asp__format_number;
			"FormatPercent", not_implemented_yet;
			"GetLocale", not_implemented_yet;
			"GetObject", not_implemented_yet;
			"GetRef", not_implemented_yet;
			"Hex", asp__hex;
			"Hour", not_implemented_yet;
			"InputBox", not_implemented_yet;
			"InStr", asp__in_str;
			"InStrRev", asp__in_str_rev;
			"Int", asp__int;
			"IsArray", asp__is_array;
			"IsDate", asp__is_date;
			"IsEmpty", asp__is_empty;
			"IsNull", asp__is_null;
			"IsNumeric", asp__is_numeric;
			"IsObject", asp__is_object;
			"Join", asp__join;
			"LBound", asp__lbound;
			"LCase", asp__lcase;
			"Left", asp__left;
			"Len", asp__len;
			"LoadPicture", not_implemented_yet;
			"Log", asp__log;
			"LTrim", asp__ltrim;
			"Mid", asp__mid;
			"Minute", not_implemented_yet;
			"Month", not_implemented_yet;
			"MonthName", not_implemented_yet;
			"MsgBox", not_implemented_yet;
			"Now", asp__now;
			"Oct", asp__oct;
			"Replace", not_implemented_yet;
			"RGB", not_implemented_yet;
			"Right", asp__right;
			"Rnd", asp__rnd;
			"Round", not_implemented_yet;
			"RTrim", not_implemented_yet;
			"ScriptEngine", not_implemented_yet;
			"ScriptEngineBuildVersion", not_implemented_yet;
			"ScriptEngineMajorVersion", not_implemented_yet;
			"ScriptEngineMinorVersion", not_implemented_yet;
			"Second", not_implemented_yet;
			"SetLocale", not_implemented_yet;
			"Sgn", not_implemented_yet;
			"Sin", asp__sin;
			"Space", asp__space;
			"Split", asp__split;
			"Sqr", not_implemented_yet;
			"StrComp", asp__str_comp;
			"String", asp__string;
			"StrReverse", not_implemented_yet;
			"Tan", asp__tan;
			"Time", not_implemented_yet;
			"Timer", asp__timer;
			"TimeSerial", not_implemented_yet;
			"TimeValue", not_implemented_yet;
			"Trim", asp__trim;
            "TypeName", asp__typename;
			"UBound", asp__ubound;
			"UCase", not_implemented_yet;
			"VarType", asp__vartype;
			"Weekday", not_implemented_yet;
			"WeekdayName", not_implemented_yet;
			"Year", not_implemented_yet;
		];

	let string_of_ints =
		(function clist -> String (
			 String.concat "" 
					                   (List.map (fun x -> String.make 1 (Char.chr x)) clist)))
	in
	List.iter
		(fun (x,y) -> add_variable runtime (Symbol.of_string x) (ref y))
		[
			(* string constants *)
			"VbCr", string_of_ints [13];
			"VbCrLf", string_of_ints [13; 10];
			"VbFormFeed", string_of_ints [12];
			"VbLf", string_of_ints [10];
			"VbNewLine", String "\n";
			"VbNullChar", string_of_ints [0];
			"VbNullString", String "";
			"VbTab", string_of_ints [9];
			"VbVerticalTab", string_of_ints [11];

			(* tristate *)
			"TristateTrue", Int (-1l);
			"TristateFalse", Int 0l;
			"TristateUseDefault", Int (-2l);
            
            (* VarTypes *)
            "vbEmpty", Int 0l;
            "vbNull", Int 1l;
            "vbInteger", Int 2l;
            "vbLong", Int 3l;
            "vbSingle", Int 4l;
            "vbDouble", Int 5l;
            "vbCurrency", Int 6l;
            "vbDate", Int 7l;
            "vbString", Int 8l;
            "vbObject", Int 9l;
            "vbError", Int 10l;
            "vbBoolean", Int 11l;
            "vbVariant", Int 12l;
            "vbDataObject", Int 13l;
            "vbByte", Int 17l;
            "vbArray", Int 8192l;
		];

	List.iter
		(fun (x,y) -> add_class runtime (Symbol.of_string x) y)
		[
			"VbScriptError", fun () -> (new vbscript_err :> object_t)
		];

	List.iter (fun (classname, objname) -> 
                   ( add_object runtime 
                         ~of_class:(Symbol.of_string classname) (Symbol.of_string objname)) )
		[
			"VbScriptError", "Err"
		]
;;

let _ =
	register_module "vbStdLib" load
;;
