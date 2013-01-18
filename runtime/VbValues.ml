
open VbTypes
open Printf

(*================== Arrays =========================*)
(* using Array.init instead of Array.make is important, because otherwise
   every element of the array is a reference to the same location! *)
let rec create_array int_list =
	match int_list with
	    | [] -> Single (Array.init 0 (fun x -> (ref Empty)))
	    | i::[] -> Single (Array.init i (fun x -> (ref Empty)))
	    | i::rest -> Multi (Array.init i (fun x -> (create_array rest)))

let rec array_index arr int_list =
	try
		match arr, int_list with
		    | Single x, i::[] -> x.(i)
		    | Multi x, i::rest -> array_index x.(i) rest
		    | _, _ -> raise (Invalid_indices (Array arr, int_list))
	with
		    Invalid_argument _ -> raise Array_out_of_bounds

(* TODO: have type-based enforcement of the equality of the
   sizes of each dimension of an array *)
let rec array_length arr dimension =
    match arr, dimension with
        | Single x, 0 -> Array.length x
        | Multi x, i -> array_length x.(0) (dimension - 1)
        | _, _ -> raise (Invalid_argument "array_length")

let max v1 v2 = if v1 > v2 then v1 else v2
let min v1 v2 = if v1 < v2 then v1 else v2

(* it is official that old the last dimension can be changed! *)
let rec merge_arrays blank old =
	match blank, old with
	    | Single snew, Single sold ->
		      for i = 0 to (min (Array.length snew) (Array.length sold)) - 1 do
			      snew.(i) <- sold.(i)
		      done;
		      Single snew
	    | Multi snew, Multi sold ->
		      for i = 0 to (min (Array.length snew) (Array.length sold)) - 1 do
			      snew.(i) <- merge_arrays snew.(i) sold.(i)
		      done; 
		      Multi snew
	    | _, _ -> raise (Failure "cant redim w/ different dimensions")

(*========== helpers to pull values out of vb variants ===============*)
(* the conversions of the value 'empty' to an int/float/string are
   probably only done for comparison operators in the original language,
   but we do it all the time i guess *)
let debug_string v =
	match v with
	    | Empty -> "Empty"
	    | Null -> "Null"
	    | Bool b -> "Bool: " ^ (string_of_bool b)
	    | DateTime (_, d) -> "Date: blah"
	    | Int i -> "Int: " ^ (Int32.to_string i)
	    | Float f -> "Float: " ^ (string_of_float f)
	    | String s -> "String: " ^ s
	    | Array a -> "Array"
	    | Object None -> "Object: Nothing"
	    | Object (Some o) -> sprintf "Object: %s" (Symbol.to_string (!o # classname))

let netdate_of_comfloat days_since_com_epoch =
    (* Netdate is seconds since 1 Jan 1970
       COM is days since 30 Dec 1899 
       
       My math says that is 25569 days difference *)
    let days_since_netdate_epoch = days_since_com_epoch -. 25_569.0 in
    Netdate.create (days_since_netdate_epoch *. 86_400.0)

let comfloat_of_netdate d =
    let seconds_since_netdate_epoch = Netdate.since_epoch d in
    let days_since_netdate_epoch = seconds_since_netdate_epoch /. 86_400.0 in
    days_since_netdate_epoch +. 25_569.0 


let current_unixfloat = Unix.time

let netdate_of_unixfloat f = Netdate.create (f +. (float_of_int Netdate.localzone) *. 60.0)

let get_bool v =
	match v with
	    | Empty | Null -> false
	    | Bool b -> b
	    | Int i -> i <> 0l
	    | String s -> (try (bool_of_string s) with _ -> (int_of_string s) <> 0)
	    | _ -> raise (Cannot_convert (v, "bool"))

let get_int32 v =
	match v with
	    | Empty | Null -> 0l
	    | Int i -> i
	    | String s ->
		      (try Int32.of_string s
		       with Failure "int_of_string" -> raise (Cannot_convert (v,"int")) )
	    | Object None -> 0l
	    | _ -> raise (Cannot_convert (v, "int"))

let get_int v =
    Int32.to_int (get_int32 v)

(* TODO: don't ignore the date label *)
let get_float v =
	match v with
	    | Float f -> f
	    | Int i -> (Int32.to_float i)
	    | String s -> (float_of_string s)
	    | Object None -> 0.0
	    | _ -> raise (Cannot_convert (v, "float"))

(* weird strings for debugging only, Null and Empty should be "" *)
let get_string v =
	match v with
	    | Null | Empty | Object None -> ""
	    | Float f -> (string_of_float f)
	    | Int i -> (Int32.to_string i)
	    | String s -> s
	    | Bool b -> (string_of_bool b)
	    | DateTime (Both, d) -> (Netdate.format "%m/%d/%Y %T" d)
	    | DateTime (Date, d) -> (Netdate.format "%m/%d/%Y" d)
	    | DateTime (Time, d) -> (Netdate.format "%T" d)
	    | _ -> raise (Cannot_convert (v, "string"))

(* TODO: parse dates with ocamlnet *)
let get_date v =
	match v with
        | Int i -> (Date, netdate_of_comfloat (Int32.to_float i))
        | Float f -> (Both, netdate_of_comfloat f)
	    | DateTime (t, d) -> (t, d)
	    | String s -> (Both, Netdate.parse s) (* FIXME Shoudln't just return Both, we need to 
											     test each date/time component. Later *)
	    | _ -> raise (Cannot_convert (v, "date"))

let get_array v =
	match v with
	    | Array a -> a
	          (* TODO: try to get object's default property here *)
	    | _ -> raise(Not_array v)

let get_object v =
	match v with
	    | Object None -> raise (Failure "dereferencing null object")
	    | Object (Some o) -> !o
	    | _ -> raise (Not_object v)

let wrap_int32 i = ref (Int i)
let wrap_int i = wrap_int32 (Int32.of_int i)
let wrap_float f = ref (Float f)
let wrap_date d = ref (DateTime d)
let wrap_string s = ref (String s)
let wrap_bool b = ref (Bool b)
let wrap_array a = ref (Array (Single a))
let wrap_object o = ref (Object (Some (ref o)))
let wrap_byte b = wrap_int b

(*==================== evaluation helpers ======================*)

let arg0 funcname args =
	match args with
	    | [] -> ()
	    | z -> raise (Invalid_arg_count (funcname, 0, List.length z))

let arg1 funcname args =
	match args with
	    | [x] -> x
	    | z -> raise (Invalid_arg_count (funcname, 1, List.length z))

let arg2 funcname args =
	match args with
	    | [x;y] -> x,y
	    | z -> raise (Invalid_arg_count (funcname, 2, List.length z))

let arg3 funcname args =
	match args with
	    | [x;y;z] -> x,y,z
	    | z -> raise (Invalid_arg_count (funcname, 3, List.length z))

let arg4 funcname args =
	match args with
	    | [x;y;z;w] -> x,y,z,w
	    | z -> raise (Invalid_arg_count (funcname, 4, List.length z))

(* TODO: make tail-recursive... like everything else :-) *)
let rec string_of_id id =
	match id with
	    | AspAst.AtomicId name -> Symbol.to_string name
	    | AspAst.Dot (base, name) -> (string_of_id base) ^ (Symbol.to_string name)
	    | AspAst.Indices (base, indices) -> (string_of_id base) ^ "()"

(* helper to give an int when possible, and a float when not, and now a datetime as the
   most specific of all :-) *)
let num_apply int_func float_func value =
	try 
		let i = (get_int32 !value) in 
		Int (int_func i)
	with Cannot_convert _ ->
		try 
			let f = (get_float !value) in 
			Float (float_func f)
		with Cannot_convert _ -> 
            try 
                let d = comfloat_of_netdate (snd (get_date !value)) in
                DateTime (Both, netdate_of_comfloat (float_func d))
            with Cannot_convert _ ->
				raise (Cannot_convert (!value, "number"))
                          
let num_apply2 int_func float_func val1 val2 =
	try 
		let i1, i2 = (get_int32 !val1), (get_int32 !val2) in 
		Int (int_func i1 i2)
	with Cannot_convert _ ->
		try 
			let f1, f2 = (get_float !val1), (get_float !val2) in 
			Float (float_func f1 f2)
		with Cannot_convert _ -> 
            try 
                let d1, d2 = 
                    comfloat_of_netdate (snd (get_date !val1)),
                    comfloat_of_netdate (snd (get_date !val2))
                in
                DateTime (Both, netdate_of_comfloat (float_func d1 d2))
            with Cannot_convert (v,t) ->
                raise (Cannot_convert (v, "number"))
                        
				        (* this traps which value it was that didn't work *)
                          
(* TODO: rather than 'matching' the int, we should try to get_int on them
   another alternative is to store as an int whenever possible, and cast
   to string only as needed for other operations *)
let bool_apply boolfunc intfunc value =
	match !value with
	    | Null -> Null
	    | Int i -> Int (intfunc i)
	    | Bool b -> Bool (boolfunc b)
	    | _ -> raise (Invalid_argument "unary boolean operator")

(* the optional params deal with combinations of bool plus null
   for example ~true_null:true will behave like vbscript's 'or' and
   a combination of true and null will be true *)
let bool_apply2 boolfunc intfunc 
	?(true_null=Null) ?(false_null=Null) val1 val2 =
	match !val1, !val2 with
	    | Null, Bool b 
	    | Bool b, Null -> if b then true_null else false_null
	    | Int i1, Int i2 -> Int (intfunc i1 i2)
	    | Bool b1, Bool b2 -> Bool (boolfunc b1 b2)
	    | _, _ -> raise (Invalid_argument "binary boolean operator")

let vb_compare val1 val2 =
	match !val1, !val2 with
	    | Int i1, Int i2 -> compare i1 i2
	    | String s1, String s2 -> compare s1 s2

	    | Int _, String _ -> compare 0 max_int
	    | String _, Int _ -> compare max_int 0

        | Null, Int i
	    | Empty, Int i -> compare 0l i
        | Int i, Null
	    | Int i, Empty -> compare i 0l

        | Null, String s
	    | Empty, String s -> compare "" s
        | String s, Null
	    | String s, Empty -> compare s ""

        | Null, Null
	    | Empty, Empty -> 0
	    | a, b -> raise (Invalid_argument (sprintf "vb_compare %S %S" (debug_string a) (debug_string b)))
	          
let xor b1 b2 = (b1 || b2) && not (b1 && b2)
let imp b1 b2 = (not b1) || b2
let limp i1 i2 = (lnot i1) lor i2
let leqv i1 i2 = (i1 land i2) lor ((lnot i1) land (lnot i2))


(*=================== functions / members =================== *)

let create_function name params statements =
	(* no optional params for user-defined funcs yet *)
	(* TODO: check byref and byval, and figure out which is default *)
    {
		f_name = name;
		f_args = params;
		f_body = statements
	}



let rec get_file_casefold filename =
    let dir = Filename.dirname filename in
    if dir = filename then
        dir
    else
        let dirpath = get_file_casefold dir in
        let files = Array.to_list (Sys.readdir dirpath) in

        let reg = Str.regexp_case_fold (Str.quote (Filename.basename filename)) in
        Filename.concat 
            dirpath 
            (List.find (fun s -> Str.string_match reg s 0) files)
        
