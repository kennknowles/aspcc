
(** 
	A currently empty module to provide ADO support 

	Here are some resources: 

	OcamlODBC: http://pauillac.inria.fr/~guesdon/Tools/ocamlodbc/ocamlodbc.html

	OcamlMySQL: http://raevnos.pennmush.org/code/ocaml.html

	PostGres: http://www.eleves.ens.fr/home/frisch/soft

	Note that this file is for the ASPCC module, wherease the file caml_ado.ml
	should be for a native caml ADO implementation that this can wrap around,
	if that proves convenient

*)

open VbValues
open VbClass
open VbTypes
open Runtime
open Printf

module StringMap = Map.Make(String)

type conn = [ `None | `Generic of Dbi.connection | `Freetds of Dbi_freetds.connection ]
type stmt = [ `Generic of Dbi.statement | `Freetds of Dbi_freetds.statement ]

type rs = {
    names : int StringMap.t;
    rows : Dbi.sql_t array array;
}

let fold_index (map, curr_index) key =
    StringMap.add (String.lowercase key) curr_index map, curr_index + 1
        

let rslist_of_sth sth =
    [
        {
            names = fst (List.fold_left 
                             fold_index 
                             (StringMap.empty, 0) 
                             (sth # names));

            rows = Array.of_list (List.map Array.of_list (sth # fetchall ()))
        } 
    ]

let rslist_of_sth_freetds sth =
    List.map 
        (fun ct_rs ->
             {
                 names = fst (Array.fold_left
                                  fold_index 
                                  (StringMap.empty, 0) 
                                  ct_rs.Dbi_freetds.rs_names);

                 rows = ct_rs.Dbi_freetds.rs_rows
             })
        sth # rs_fetch_all


(** {4 Helper stuff} *)
let not_supported name =
    (fun _ -> raise (Not_implemented (sprintf 
                                          "ASPCC's ADO does not support method %s"
                                          name )))



(** {4 ADO classes} *)

class field =
object (self)
	inherit opaque_object

	val mutable attributes = 0

    method strname = "ADO.Field"

	(** [Field.ActualSize] would normally return the size of the
		value of the field, but zero is the default value, and that is
		all this one returns :)  Another acceptable return value is
		adUnknown *)
	method actual_size_get = function params ->
		arg0 "ADO.Field.ActualSize" params;
		wrap_int 0

	(** [Field.Attributes] is a bit-field of flags on the field.  Right now
		ours returns zero all the time *)
	method attributes_get = function params ->
		arg0 "ADO.Field.Attributes (get)" params;
		wrap_int 0

	(** [Field.Attributes = value] should set the attributes if our fields
		were modifiable, which they aren't right now. *)
	method attributes_let = function params ->
		let attr = arg1 "ADO.Field.Attributes (let)" params in
		attributes <- get_int !attr;
		ref Null;

	(** [Field.DefinedSize] will tell you what the maximum size for a field
		is, as opposed to its current size *)
	method defined_size_get = function params ->
        arg0 "Field.DefinedSize" params;
		wrap_int 0
(*
    method property ?(action = Get) name =
		match action, String.lowercase name with
		| _, "" -> raise (Symbol_not_found "Default property for ADO.Field object")

		(* Properties *)
		| Get, "actual_size" -> Builtin self#actual_size_get
(*		| Get, "attributes" -> Builtin self#actual_size_get
		| Let, "attributes" -> Builtin self#actual_size_let *)

		(* Methods *)
(*		| Get, "clear" -> Builtin self#clear_method *)
(*		| Get, "end" -> Builtin self#end_method *)
(*		| Get, "flush" -> Builtin self#flush_method *)
(*		| Get, "write" -> Builtin self#write_method*)

        | _, s -> raise (Symbol_not_found ("Field." ^ s))
*)
end

and recordset =
    let method_id s = "ADO.RecordSet." ^ s in
object(self)
    inherit opaque_object
    method strname = "ADO.RecordSet"

    val mutable recordsets = [||]
    val mutable curr_row = -1
    val mutable curr_rs = -1

    method load (sth : stmt) =
        (match sth with
             | `Generic g -> recordsets <- Array.of_list (rslist_of_sth g)
             | `Freetds f -> recordsets <- Array.of_list (rslist_of_sth_freetds f)
             | `None -> failwith "Cannot load `None sth type");
        curr_rs <- 0;
        curr_row <- 0

    method field_names =
        let namelist = 
            StringMap.fold 
                (fun key value li -> (wrap_string key)::li) 
                recordsets.(curr_row).names 
                [] 
        in
        Array.of_list namelist

    method get fieldname =
        try
            let rs = recordsets.(curr_rs) in
            rs.rows.(curr_row).(
                StringMap.find (String.lowercase fieldname) rs.names
            )
        with
            | Invalid_argument _ -> Runtime.raise_error "Either BOF or EOF is true."
            | Not_found -> 
                  Runtime.raise_error (sprintf "Not found in recordset: '%s'" fieldname)
                  
    method m_gets name params =
        match name with
            | ""
            | "fields" -> 
                  ( match params with
                        | [] -> wrap_object (new collection "Fields" (self # field_names) :> object_t)
                        | [s] -> wrap_string (Dbi.sql_t_to_string (self # get (get_string !s)))
                        | li -> raise (Invalid_arg_list "Recordset.Fields")
                  )
                            
            | "bof" ->
                  arg0 (method_id "BOF") params;
                  wrap_bool (recordsets.(curr_rs).rows = [||])
                      
            | "eof" ->
                  arg0 (method_id "EOF") params;
                  wrap_bool (curr_row >= Array.length recordsets.(curr_rs).rows)
                      
		    (* Methods *)
            | "movenext" ->
                  arg0 (method_id "MoveNext") params;
                  curr_row <- curr_row + 1;
                  ref Null
                  
            | "nextrecordset" -> 
                  (* LOLLERZ I always whined about MS having the same
                     restriction I just placed by this logic *)
                  if curr_rs + 1 >= Array.length recordsets then
                      ref (Object None)
                  else
                      wrap_object ({< curr_rs = curr_rs + 1>} :> object_t)
                          
            | "state" ->
                  arg0 (method_id "State") params;
                  if recordsets = [||]
                  then wrap_int 0
                  else wrap_int 1

                  | _ -> self # not_found name params

(*    method m_lets name params =
        match name with
            | "" -> self # fields_get params
            | s -> self # not_found s params*)
end

and connection =
    let parse_connstring s =
        let semicolon = Str.regexp ";" in
        let equals = Str.regexp "[ \t]*=[ \t]*" in
        let entries = Str.split semicolon s in
        List.fold_left 
            (fun mapping entry ->
                 match Str.split equals entry with
                     | [name; value] -> (String.lowercase name, value) :: mapping
                     | [name] -> (String.lowercase name, "") :: mapping
                     | _ -> raise (Runtime.Error (sprintf "Invalid connection string: '%s'" s)))
            []
            entries
    in

    let method_id s = "ADO.Connection." ^ s in
object(self)
    inherit opaque_object

    method strname = "ADO.Connection"

                         
    val mutable dbi_connection = (`None : conn)
    val mutable command_timeout = 30
    val mutable connection_string = ""

    method private my_conn =
        match dbi_connection with
            | `None -> Runtime.raise_error "Connection not open"
            | `Generic conn -> conn
            | `Freetds fconn -> (fconn :> Dbi.connection)
                  
    method open_conn params =
        let connstring, rest = match params with
            | [] -> raise (Invalid_arg_count ("Connection.Open", 1, 0))
            | connstring :: rest -> connstring, rest
        in
        
        let alist = parse_connstring (get_string !connstring) in

        let maybe_assoc key li = try Some (List.assoc key li) with Not_found -> None in

        let provider, database =
            List.assoc "provider" alist,
            List.assoc "database" alist
        in

        let host, user, password =
            match rest with
                | [] ->
                      maybe_assoc "server" alist,
                      maybe_assoc "uid" alist,
                      maybe_assoc "pwd" alist

                | [connstring; userid] ->
                      maybe_assoc "server" alist,
                      Some (get_string !userid),
                      maybe_assoc "pwd" alist
                      
                | [connstring; userid; pwd] ->
                      maybe_assoc "server" alist,
                      Some (get_string !userid),
                      Some (get_string !pwd)
                      
                | _ -> raise (Invalid_arg_count ("Connection.Open", 3, List.length params))
        in
        
        
        (dbi_connection <- match String.lowercase provider with
            | "sqloledb"  
            | "oledb"  
            | "freetds" -> `Freetds (new Dbi_freetds.connection ?host ?user ?password database)
            | s -> `Generic (Dbi.Factory.connect provider ?host ?user ?password database));
        
        ref Null
            
    method m_gets name params =
        match name with
            | "commandtimeout" ->
                  arg0 "Connection.CommandTimeout Get" params;
                  wrap_int command_timeout

            | "connectionstring" -> 
                  arg0 "Connection.ConnectionString Get" params;
                  ref (String connection_string)
		
            (* Methods *)
		    | "close" ->
                  arg0 "ADO.Connection.Close" params;
                  self # my_conn # close ();
                  ref Null

		    | "execute" ->
                  let sql = get_string !(arg1 (method_id "Execute") params) in
                  let rs = new recordset in
                  ( try
                        match dbi_connection with
                            | `None -> Runtime.raise_error "Connection not open."
                            | `Generic g ->
                                  let sth = g # ex sql [] in
                                  rs # load (`Generic sth);
                                  wrap_object (rs :> VbTypes.object_t)
                                      
                            | `Freetds f ->
                                  let sth = f # ex_multi sql [] in
                                  rs # load (`Freetds sth);
                                  wrap_object (rs :> VbTypes.object_t)
                    with
                        | Dbi.SQL_error s ->
                              Runtime.raise_error (sprintf "Statement failed:\n %s\n%s" sql s) )
                  
            | "open" -> self # open_conn params
             
            | _ -> 
                  let paramstrings = List.map (fun v -> get_string !v) params in
                  let sql = sprintf "EXEC %s %s" name (String.concat "," paramstrings) in
                  self # m_gets "execute" [wrap_string sql]
     
            | _ -> self # not_found name params
                  
    method m_lets name params =
        (match name with
		     (* Properties *)
		     | "commandtimeout" -> 
                   command_timeout <- get_int !(arg1 (method_id "CommandTimeout Let") params);
                   
             | "connectionstring" -> 
                   connection_string <- get_string !(arg1 "Connection.ConnectionString Let" params);
                   
             | _ -> ignore (self # not_found name params));
        ref Null
end

let load runtime =
	List.iter (fun (x,y) -> Runtime.add_variable runtime (Symbol.of_string x) (ref y))
		[
			(* Cursor types *)
			"adOpenForwardOnly", Int 0l;

			(* Tristate constants *)
			"TristateTrue", Int (-1l);
			"TristateFalse", Int 0l;
			"TristateUseDefault", Int (-2l)
		];

    Runtime.add_class runtime 
        (Symbol.of_string "ADODB.Connection")
        (fun () -> (new connection :> object_t));

    Runtime.add_class runtime 
        (Symbol.of_string "ADO.Connection")
        (fun () -> (new connection :> object_t));

    Runtime.add_class runtime 
        (Symbol.of_string "Connection")
        (fun () -> (new connection :> object_t));
    
    Runtime.add_class runtime 
        (Symbol.of_string "RecordSet")
        (fun () -> (new connection :> object_t));
    
    Runtime.add_class runtime 
        (Symbol.of_string "ADODB.RecordSet")
        (fun () -> (new connection :> object_t))

let _ =
    register_module "ado" load
        
