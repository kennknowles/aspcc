
(** ASPCC module with fake ASP intrinsics objects intended for output to
	the command line *)

(**
	This file has a few methods from ASP intrinsic objects that are
	meaningful on the command line, and it is meant to be a 'mockup'
	of the real ASP objects, for testing purposes

	For example, the only output function I know of for VbScript is
	Response.Write (presumably you could have another embedded object with
	a 'Write' method)

	Many of these functions are directly lifted from aspIntrinsics.ml, but
	aspIntrinsics.ml is being merged with Mod_aspcc, since many of the
	methods have to be instantiated for the current page.  The documentation
	below is documentation for the actual behavior of this library, rather
	than what they "would" do in a web-based situation.

	Note also that formal parameter lists will given as though they were
	in VbScript, even though they are actually passed as a
	[VbTools.vb_value list]
*)

open VbTools
open VbTypes
open Run
open Neturl
		
(** TODO: move this into vbTools or something *)
let invalid_arg_count funcname num li =
	raise (Invalid_arg_count (funcname, num, List.length li))

let env_mockup_syntax = {
	null_url_syntax
	with
		url_enable_query = Url_part_allowed;
		url_enable_fragment = Url_part_allowed
}

let file_url_syntax = {
	(Hashtbl.find Neturl.common_url_syntax "http") with
		url_enable_scheme = Url_part_not_recognized
}
	


(* {4 Placeholder functions} *)

(** For flagging unimplemented functions *)
let not_implemented_yet =  
    BuiltinMember (fun _ -> raise (Not_implemented "builtin function"))
	

(** A member dispatch where 'get' 'set' and 'let' are all 
[not_implemented_yet] *)
let not_implemented_member = {
	m_let = Some (Asp.Public, not_implemented_yet);
	m_set = Some (Asp.Public, not_implemented_yet);
	m_get = Some (Asp.Public, not_implemented_yet)
}

let not_implemented =
    Builtin (fun _ -> raise (Not_implemented "builtin function"))


(** {4 Response} *)

(** The class of response objects *)
class response = 
object (self)
	(* TODO: implement the buffering myself, can't do it with stdout,
		use stdlib Buffer *)

	(** Flag to indicate whether any output has been sent to the client *)
	val mutable have_output = false

	(** If [have_output] is true, then it is illegal to change this *)
	val mutable buffer_on = false

	(** Internal buffer *)
	val buffer = Buffer.create 80

	
	(*=================== properties =======================*)

	(** [Response.buffer] indicates whether stdout will be buffered *)
	method private buffer_get = function params ->
		match params with
		| [] -> ref (Bool buffer_on)
		| z -> invalid_arg_count "Response.Buffer" 0 z

	(** [Response.buffer = bool] sets the buffer on/off, but cannot be
		changed once any output has been sent to the client *)
	method private buffer_let = function params ->
		match params with
		| [x] -> 
			if have_output then
				raise (Failure ("Cannot change the value of Response.Buffer " ^
								"after data has been output."));
			buffer_on <- (get_bool !x); 
			ref Null
		| _ -> raise (Invalid_arg_list "Response.Write")


	(*===================== methods =======================*)
	
	(** [Response.Clear] clears the buffer if buffering is on *)	
	method private clear_method = function params ->
		match params with
		| [] ->
			if buffer_on then
				(Buffer.clear buffer; ref Null)
			else
				raise (Failure ("Cannot clear Response buffer when buffering " ^
								"is not enabled"))
		| z -> invalid_arg_count "Response.Clear" 0 z

	(** [Response.end] exits execution of the script *)
	method private end_method = function params ->
		match params with
		| [] -> 
			if buffer_on then print_string (Buffer.contents buffer);
			exit 0
		| z -> invalid_arg_count "Response.End" 0 z

	(** [Response.flush] outputs everything in the buffered response *)
	method private flush_method = function params ->
		match params with
		| [] ->
			if buffer_on then
				(print_string (Buffer.contents buffer); 
				Buffer.clear buffer;
				ref Null)
			else
				raise (Failure ("Cannot flush Response buffer when buffering " ^
								"is not enabled"))
		| z -> invalid_arg_count "Response.Clear" 0 z

	(** [Response.Write( str )] writes [str] to stdout *)
	method private write_method = function params ->
		match params with
		| [x] -> 
			have_output <- true;
			if buffer_on then
				Buffer.add_string buffer (get_string !x)
			else
				print_string (get_string !x);
			ref Null
		| z -> invalid_arg_count "Response.Write" 0 z 


	(** There are no accessable fields, so Symbol_not_found is
		immediately raised *)
	method field name = raise (Symbol_not_found name); ref Null
	
	(** See [field] *)
	method set_field name (value:value_t ref) : unit = 
		raise (Symbol_not_found name) 


	(*=========== Constructor/destructior ==============*)
	method private destructor = function params ->
		if buffer_on then print_string (Buffer.contents buffer);
		ref Null

	method property ?(action = Get) name =
	match action, String.lowercase name with
	| _, "" -> raise (Symbol_not_found "Default property for 'Response' object")

	(* Destructor *)
	| Get, "class_destroy" -> Builtin self#destructor

	(* Properties *)
	| Get, "buffer" -> Builtin self#buffer_get
	| Let, "buffer" -> Builtin self#buffer_let

	(* Methods *)
	| Get, "clear" -> Builtin self#clear_method
	| Get, "end" -> Builtin self#end_method
	| Get, "flush" -> Builtin self#flush_method
	| Get, "write" -> Builtin self#write_method
					
(*					(* Properties *)
					"CacheControl", not_implemented_member;
					"Charset", not_implemented_member;
					"ContentType", not_implemented_member;
					"Expires", not_implemented_member;
					"ExpiresAbsolute", not_implemented_member;
					"IsClientConnected", not_implemented_member;
					"PICS", not_implemented_member;
					"Status", not_implemented_member;

					(* Collections *)
					"Cookies", not_implemented_member;
				
					(* Methods *)
					"AddHeader", not_implemented_member;
					"AppendToLog", not_implemented_member;
					"BinaryWrite", not_implemented_member;
					"Redirect", not_implemented_member;
					*)

	| _, name -> raise (Symbol_not_found ("Response." ^ name))

end

(** The class that generates response objects *)
class response_class = object
	method create_object () =
		let x = (new response) in (x :> object_t)
end
;;

(** {4 Request} *)

(** The request object parses the environment variables [GET] and [POST] as
	though they were CGI querystrings.  For example [hello.asp?foo=1&baz=2]
	would set [foo] to [1] and [baz] to [2].  All request data is initially
	a string. *)

(** [Request.QueryString( key )] returns the [key] variable parsed out of
	the [GET] environment variable *)
let asp__request_querystring self params =
	match params with
	| [x] -> ref (String "Querystring not implemented yet"); 
	| _ -> raise (Invalid_arg_list "Request.QueryString")


(** {4 Server} *)

(** The Server object is parameterized by the runtime; this is to avoid passing
	the runtime to every builtin function.  If we passed the runtime to every
	bultin function, then Eval could be implemented without being a special
	case, but a lot of craziness might ensue! (maybe this is a good thing)
	In any event, it would be a lot of editing :-) *)
class server the_runtime =
object(self)
	inherit opaque_object
	val runtime = the_runtime

	(** [Server.CreateObject("className")] is the same as [new className] *)
	method private create_object_method params =
	match params with
	| [x] -> 
		let the_class = 
			(get_symbol (Run.get_builtin_scope runtime).classes (get_string !x)) 
		in
			ref (Object (Some (ref
					(the_class#create_object ()))))
	| z -> invalid_arg_count "Server.CreateObject" 1 z

	(** [Server.MapPath("RelativePathFromScript")] 
		returns an absolute path to the file, so that if you invoke ASPCC like
		[aspcc some/dir/script.asp], it will still find the file relative
		to the script rather than the invocation directory *)
	method private map_path_method params =
		let relpath = (get_string !(arg1 "Server.MapPath" params)) in
		ref (String (	
				(*"file://" ^   <-- I don't think this is needed/appropriate *)
				(Sys.getenv "PWD") ^ "/" ^
				(Filename.dirname (Run.get_script_path runtime)) ^ "/" ^
				relpath
			))

	method property ?(action = Get) name =
	match action, String.lowercase name with
	| _, "" -> raise (Symbol_not_found "Default property for 'Server' object")

	(* Property *)
	| Get, "ScriptTimeout" -> not_implemented
	| Let, "ScriptTimeout" -> not_implemented
					
	(* Methods *)
	| Get, "createobject" -> Builtin self#create_object_method 
	| Get, "Execute" -> not_implemented
	| Get, "GetLastError" -> not_implemented
	| Get, "HtmlEncode" -> not_implemented
	| Get, "mappath" -> Builtin self#map_path_method
	| Get, "Transfer" -> not_implemented
	| Get, "UrlEncode" -> not_implemented
	
	| _, name -> raise (Symbol_not_found ("Server." ^ name))

end
;;

class server_class the_runtime = object
	val runtime = the_runtime
	method create_object () =
		let x = (new server runtime) in (x :> object_t)
end
;;

(** [Server.HtmlEncode str] escapes any key characters for html, for
	example changing [<] to [&gt;] and [&] to [&amp;] *)
let asp__server_html_encode self params = 
	match params with
	| [x] -> 
		let result = Nethtml.encode [(Nethtml.Data (get_string !x))] in
		ref (String (String.concat "" (
			List.map
				(function Nethtml.Data d -> d
					| _ -> raise (Failure "My bad, broke Neththml.encode"))
				result
		)))
	| _ -> raise (Invalid_arg_list "Server.HtmlEncode")
    
(** {4 System} *)

(** The System object I've made up just for the console, to make getting at argv
    easier.  Any script that uses argv won't work on the web, of course *)
class system the_runtime =
object(self)
	inherit opaque_object
	val runtime = the_runtime

    (** [System.Argv(x) returns the xth entry in argv.  TODO: return the whole
    array if no index is specified *)
	method private argv_get params =
        let x = (get_int !(arg1 "System.Argv" params)) in
        ref (String (Sys.argv.(x+2)))

    (** [System.Argc] returns the arg count, until i can just take
     ubound(System.argv) *)
	method private argc_get params =
        arg0 "System.Argc" params;
        ref (Int ((Array.length Sys.argv) - 2))

	method property ?(action = Get) name =
	match action, String.lowercase name with
	| _, "" -> raise (Symbol_not_found "Default property for 'System' object")

	(* Property *)
	| Get, "argv" -> Builtin self#argv_get
	| Get, "argc" -> Builtin self#argc_get
	
	| _, name -> raise (Symbol_not_found ("System." ^ name))

end
;;

class system_class the_runtime = object
	val runtime = the_runtime
	method create_object () =
		let x = (new system runtime) in (x :> object_t)
end
;;

(** {4 Loader Function} *)

let load runtime =
	Run.set_html_func runtime Pervasives.print_string;
(*	List.iter
		(fun (name,fields,funcs,default) ->
			Run.add_transparent_class runtime name fields funcs default)
		[
			(*=============== Intrinsic ASP classes =============*)
			"Application", 
				[],
				[
					(* Collection Properties *)
					"Contents", not_implemented_member;
					"StaticObjects", not_implemented_member;
					(* See session for discussion of Contents submethods *)

					(* Methods *)
					"Lock", not_implemented_member;
					"Unlock", not_implemented_member
				], 
				Some "Contents";
				
			"AspError",
				[],
				[
					(* Properties *)
					"AspCode", not_implemented_member;
					"AspDescription", not_implemented_member;
					"Category", not_implemented_member;
					"Column", not_implemented_member;
					"Description", not_implemented_member;
					"File", not_implemented_member;
					"Line", not_implemented_member;
					"Number", not_implemented_member;
					"Source", not_implemented_member
				],
				None;

			(* No implementation of ObjectContext because I don't know wtf
				it is *)

			"Request",
				(* fields *)
				["m_QueryString", (Asp.Private, None)], 

				(* dispatch *)
				[
					(* Property *)
					"TotalBytes", not_implemented_member;

					(* Collections *)
					"ClientCertificate", not_implemented_member;
					"Cookies", not_implemented_member;
					"Form", not_implemented_member;
					"QueryString", {
						m_let = None;
						m_set = None;
						m_get = Some (Asp.Public,
								BuiltinMember asp__request_querystring);
					};
					"ServerVariables", not_implemented_member;

					(* Method *)
					"BinaryRead", not_implemented_member
				],

				(* default should be changed to checking first querystring then
					form... possibly appending the results :( *)
				Some "QueryString";
			
			
			"Server",
				[], 
				[
					(* Property *)
					"ScriptTimeout", not_implemented_member;

					(* Methods *)
					"CreateObject", not_implemented_member;
					"Execute", not_implemented_member;
					"GetLastError", not_implemented_member;
					"HtmlEncode", {
						m_let = None;
						m_set = None;
						m_get = Some (Asp.Public,
									BuiltinMember asp__server_html_encode)
					};
					"MapPath", not_implemented_member;
					"Transfer", not_implemented_member;
					"UrlEncode", not_implemented_member
				], 
				None;

			"Session", [], 
				[
					(*	All but contents/sessionid/abandon/timeout are outside
						of my experience *)
					"CodePage", not_implemented_member;
					"LCID", not_implemented_member;
					"SessionID", not_implemented_member;
					"Timeout", not_implemented_member;
					"Contents", not_implemented_member;
					"StaticObjects", not_implemented_member;
					"Abandon", not_implemented_member;

					(* TODO: come up with a framework whereby we can have objects
						as members - we need deferred construction again, 
						i think.
						Contents.Remove and Contents.RemoveAll *)

					(* I don't  intend to support events unless really really
					necessary, so hard-code the on-start and on-end events *)
				],
				Some "Contents";
			

		];
*)
	Run.add_opaque_class runtime "Response" (new response_class);
	Run.add_opaque_class runtime "Server" (new server_class runtime);
	Run.add_opaque_class runtime "System" (new system_class runtime);

	List.iter (fun (obj,classname) -> 
				Run.add_builtin_object runtime obj classname)
		[
		(*=========== Intrinsic ASP objects ==========*)
	(*		"Application", "application";*)
			"Response", "response";
			"Server", "server";
            "System", "system";
	(*		"Request", "request";
			"Server", "server";
			"Session", "session"*)
		]

let _ =
	Loader.register_module "aspConsole" load
