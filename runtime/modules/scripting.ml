
(** ASPCC module with scripting runtime objects: 
  dictionary, filesystem, regexp, textstream *)

(** The err object could possibly be here, but should probably be
  instrinsic to the runtime... it probably breaks the rules *)

open VbValues
open VbClass
open VbTypes
open Runtime
open Printf

(* {4 Placeholder functions} *)

(** For flagging unimplemented functions *)
let not_implemented =  
	(fun _ -> raise (Not_implemented "builtin function"))

(** {4 Scripting.Dictionary} *)

(* These are the same as in vbStdLib, but I want the two modules to be
   independently loadable... these could go in a 3rd file, and then each
   module could contain the third file *)
type compare_mode =
        VbTextCompare | VbBinaryCompare | VbDatabaseCompare

let compare_mode_of_int i =
    match i with 
        | 0 -> VbBinaryCompare
        | 1 -> VbTextCompare
        | 2 -> VbDatabaseCompare (** I don't even really know what this means *)
        | _ -> raise (Failure "Invalid compare mode integer")

let int_of_compare_mode c =
	match c with
        | VbBinaryCompare -> 0
        | VbTextCompare -> 1
	    | VbDatabaseCompare -> 2
        | _ -> raise (Failure "Invalid compare mode variant")

exception Not_empty

(** Scripting.Dictionary class *)

(*	Implementation notes:  The 'self' object is bound when a method is
	called, so all of those methods that return functions with self in them
	*will* work properly... it is a little funky but easier than creating
	functions that take a 'self' parameter. *)

class dictionary =
object (self)
	inherit opaque_object
	val mutable my_compare_mode = VbBinaryCompare
	val my_hash = Hashtbl.create 10

    method strname = "Scripting.Dictionary"

	method private empty () =
		try Hashtbl.iter (fun x y -> raise Not_empty) my_hash; true
		with Not_empty -> false
            
	method private key k =
		match my_compare_mode with 
		    | VbTextCompare -> String.lowercase k
		    | _ -> k
	              
	method private compare_mode_get params =
		match params with
		    | [] -> wrap_int (int_of_compare_mode my_compare_mode)
		    | _ -> raise (Invalid_arg_list "Scripting.Dictionary.CompareMode")
                  
	(** [dict.CompareMode = x] sets the compare mode to [x], where [x] is
	  of type [compare_mode].  It is an error to change the compare mode
	  once the dictionary is non-empty (Think about it... you might be
	  merging entries!) *)
	method compare_mode_let = function params ->
		match params with
		    | [x] ->
			      if self#empty () then
				      (my_compare_mode <- compare_mode_of_int (get_int !x);
				       ref Null)
			      else
				      raise (Failure "Cannot set compare mode of non-empty dictionary")
		    | _ -> raise (Invalid_arg_list "Scripting.Dictionary.CompareMode")
                  
	method count_get = function params ->
        arg0 "Scripting.Dictionary.Count";
        wrap_int (Hashtbl.fold (fun key value accum -> accum + 1) my_hash 0)
            
	method item_get = function params ->
        let key = self # key (get_string !(arg1 "Scripting.Dictionary.Item (get)" params)) in
        try ref (Hashtbl.find my_hash key)
        with Not_found -> wrap_string ""

	(** [dict.item(key) = value] adds an entry [key, value] to the dictionary,
	  replacing any existing entries *)
	method item_let_set = function params ->
		match params with
		    | [name; value] -> 
			      Hashtbl.replace my_hash (self#key (get_string !name)) !value;
			      ref Null
		    | z -> raise (Invalid_arg_count 
			                  ("Scripting.Dictionary.Item (let)", 2, List.length z))

	(** [dict.key(oldkey) = newkey] replaces the entry [oldkey,value] with
	  the entry [newkey,value]; it just creates the new entry if the old
	  entry does not exist *)
	method key_let = function params ->
		match params with
		    | [oldkey; newkey] ->
			      let value = 
				      try
					      Hashtbl.find my_hash (self#key (get_string !oldkey))
				      with
					          Not_found -> Null
			      in
			      Hashtbl.replace my_hash (self#key (get_string !newkey)) value;
			      Hashtbl.remove my_hash (self#key (get_string !oldkey));
			      ref Null

		    | _ -> raise (Invalid_arg_list "Scripting.Dictionary.Item")

	(** [dict.Add(key, value)] is exactly equivalent to 
	  [dict.Item(key) = value] *)

	(** [dict.exists(key)] checks if anything is assigned to [key] *)

    method default_get params =
        match params with
            | [] -> self # keys_method []
            | [x] -> self # item_get [x]
            

	method exists_method params =
        let name = get_string !(arg1 "Scripting.Dictionary.Exists" params) in
        wrap_bool (Hashtbl.mem my_hash name)

	method items_method params =
        arg0 "Scripting.Dictionary.Items" params;
        wrap_array (Array.of_list (Hashtbl.fold
                                       (fun key value accum -> (ref value) :: accum)
                                       my_hash []))


	method keys_method params = 
        arg0 "Scripting.Dictionary.Keys" params;
        wrap_array (Array.of_list (Hashtbl.fold 
                                       (fun key value accum -> (wrap_string key) :: accum) 
                                       my_hash []))
            
	method remove_method params =
        let name = get_string !(arg1 "Scripting.Dictionary.Remove" params) in
        Hashtbl.remove my_hash name;
        ref Null

	method removeall_method params =
        arg0 "Scripting.Dictionary.RemoveAll" params;
        Hashtbl.clear my_hash;
        ref Null

	(** The default method is Item if one parameter, or Keys if no params 
	  You can only 'let' or 'set' the first one *)
                      
    method m_gets name =
        match name with
        | "" -> self # default_get
        | "comparemode_get" -> self # compare_mode_get
        | "count" -> self # count_get
        | "item" -> self # item_get
        
        (* "methods" *)
        | "add" -> self # item_let_set
        | "exists" -> self # exists_method
        | "items" -> self # items_method
        | "keys" -> self # keys_method
        | "remove" -> self # remove_method
        | "removeall" -> self # removeall_method

    method m_lets name =
        match name with
            | "item" -> self # item_let_set
            | "comparemode" -> self # compare_mode_let
            | "key" -> self # key_let
                  
    method m_sets name =
        match name with
            | ""
            | "item" -> self # item_let_set
end
;;

(** {4 Scripting.FileSystemObject} *)

let mode_of_int32 m =
    match m with
        | 1l -> `Read
        | 2l -> `Write
              (* | 8l -> `Append *)
        | _ -> raise (Invalid_argument "openflag_of_mode")

class textstream mode filename =
object(self)
    inherit opaque_object
    method strname = "TextStream"

    val channel = match mode with
        | `Read -> `InChannel (open_in filename)
        | `Write -> `OutChannel (open_out filename)

    method m_gets name params =
        match name with
            | "readall" ->
                  arg0 "TextStream.ReadAll" params;
                  (match channel with
                       | `OutChannel _ -> failwith "Tried to read from output channel"
                       | `InChannel ch ->
                             let b = Buffer.create 100 in
                             while
                                 try
                                     Buffer.add_string b (input_line ch);
                                     Buffer.add_char b '\n';
                                     true
                                 with
                                     | End_of_file -> false
                             do () done;
                             wrap_string (Buffer.contents b))

            | _ -> self # not_found name params
                                
end

class file path =
object(self)
    inherit opaque_object
    method strname = "Scripting.File"

    method m_gets name params =
        match name with
            | "datelastmodified" ->
                  wrap_date (Both, Netdate.create (Unix.stat path).Unix.st_mtime)

            | "openastextstream" ->
                  let mode =
                      match params with
                          | [] -> `Read
                          | [v] 
                          | [v; _] -> mode_of_int32 (get_int32 !v)
                                
                          | _ -> raise (Invalid_arg_count ("Scripting.File.OpenAsTextStream", 
                                                           2,
                                                           List.length params))
                  in
                  wrap_object (new textstream mode path :> object_t)


            | _ -> self # not_found name params
end

class file_system_object =

    let trim_all_slashes s =
        Pcre.replace 
            ~pat:"[\\/]+$"
            ~templ:""
            s
    in

    let chop_any_extension s =
        try Filename.chop_extension s
        with | Invalid_argument _ -> s
    in

object(self)

	inherit opaque_object

    method strname = "Scripting.FileSystemObject"


    method get_folder params =
        let foldername = get_string !(arg1 "Scripting.FileSystemObject.GetFolder" params) in
        (*wrap_object (new folder :> object_t)*)
        ref Null

    (* I'm using Str instead of Filename because microsoft's definition of basename
       and parent dir are funky *)
    method get_base_name params =
        let filename = get_string !(arg1 "Scripting.FileSystemObject.GetBaseName" params) in

        wrap_string (chop_any_extension (Filename.basename (trim_all_slashes filename)))

    method get_parent_folder_name params =
        let filename = get_string !(arg1 "Scripting.FileSystemObject.GetFolder" params) in

        wrap_string (Filename.dirname (trim_all_slashes filename))

    method m_gets name params =
        match name with
            | "getfile" -> 
                  let filename = get_string !(arg1 "FileSystemObject.GetFile" params) in
                  ( try
                        let path = get_file_casefold filename in
                        if Sys.file_exists path then
                            wrap_object (new file path :> object_t)
                        else
                            raise Not_found
                    with
                        | Not_found -> 
                              Runtime.raise_error (sprintf "File does not exist: %s" filename) )
                  
            | "getfolder" -> self # get_folder params
            | "getbasename" -> self # get_base_name params
            | "getparentfoldername" -> self # get_parent_folder_name params

            | _ -> self # not_found name params

(*	method property ?(action = Get) name =
	match action, String.lowercase name with

(* Properties *)
	| Get, "drives" -> Builtin not_implemented

(* Methods *)
	| Get, "buildpath" -> Builtin not_implemented
	| Get, "copyfile" -> Builtin not_implemented
	| Get, "copyfolder" -> Builtin not_implemented
	| Get, "createfolder" -> Builtin not_implemented
	| Get, "createtextfile" -> Builtin not_implemented
	| Get, "deletefile" -> Builtin not_implemented
	| Get, "deletefolder" -> Builtin not_implemented
	| Get, "driveexists" -> Builtin not_implemented
	| Get, "fileexists" -> Builtin not_implemented
	| Get, "folderexists" -> Builtin not_implemented
	| Get, "getabsolutepathname" -> Builtin not_implemented
	| Get, "getbasename" -> Builtin not_implemented
	| Get, "getdrive" -> Builtin not_implemented
	| Get, "getdrivename" -> Builtin not_implemented
	| Get, "getextensionname" -> Builtin not_implemented
	| Get, "getfile" -> Builtin not_implemented
	| Get, "getfileversion" -> Builtin not_implemented
	| Get, "getfilename" -> Builtin not_implemented
	| Get, "getfolder" -> Builtin not_implemented
	| Get, "getparentfoldername" -> Builtin not_implemented
	| Get, "getspecialfolder" -> Builtin not_implemented
	| Get, "getstandardstream" -> Builtin not_implemented
	| Get, "gettempname" -> Builtin not_implemented
	| Get, "movefile" -> Builtin not_implemented
	| Get, "movefolder" -> Builtin not_implemented
	| Get, "opentextfile" -> Builtin not_implemented *)
end
;;
(*
  class drive =
  object(self)

  inherit opaque_object

  end
  ;;

  class file =
  object(self)

  inherit opaque_object

  end
  ;;

  class folder =
  object(self)

  inherit opaque_object

  end
  ;;

  class text_stream =
  object(self)

  inherit opaque_object

  end
  ;;
*)

(** {4 Loader Function} *)

let load runtime =
	Runtime.add_class runtime 
        (Symbol.of_string "Scripting.Dictionary")
        (fun () -> (new dictionary :> object_t));

	Runtime.add_class runtime 
        (Symbol.of_string "Dictionary")
        (fun () -> (new dictionary :> object_t));

    Runtime.add_class runtime 
        (Symbol.of_string "Scripting.FileSystemObject")
        (fun () -> (new file_system_object :> object_t));
    
    Runtime.add_class runtime 
        (Symbol.of_string "FileSystemObject")
        (fun () -> (new file_system_object :> object_t))
        
let _ =
	register_module "scripting" load
