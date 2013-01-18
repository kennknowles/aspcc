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

open VbValues
open VbClass
open VbTypes
open Runtime
open Neturl
open Printf
		
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
let ignore_for_now params = ref Null	

let not_implemented =
    (fun _ -> raise (Not_implemented "builtin function"))


(** {4 Response} *)

(** The class of response objects *)
class response (is_http : bool) (really_output : string -> unit)= 
object (self)
    inherit opaque_object

	val mutable content_type = "text/html"
	val mutable cache_control = "no-cache"
	val mutable other_headers = []

	(** Flag to indicate whether any output has been sent to the client *)
	val mutable have_output = false

	(** If [have_output] is true, then it is illegal to change this *)
	val mutable buffer_on = false

	(** Internal buffer *)
	val buffer = Buffer.create 80

    method strname = "Response"
                           
	method private print s =
		if not have_output then
			(have_output <- true;
			really_output (Printf.sprintf "Content-type: %s\n" content_type);
			really_output (Printf.sprintf "Cache-control: %s\n" cache_control);
			really_output "\n";
			);
		really_output s
	
	(*=================== properties =======================*)

	(** [Response.buffer] indicates whether stdout will be buffered *)
	(** [Response.buffer = bool] sets the buffer on/off, but cannot be
		changed once any output has been sent to the client *)
	(** [Response.ContentType] sets the Content-type HTTP header *)

	(*===================== methods =======================*)
	
	(** [Response.Clear] clears the buffer if buffering is on *)	
	(** [Response.end] exits execution of the script *)
	(** [Response.flush] outputs everything in the buffered response *)
	(** [Response.Write( str )] writes [str] to stdout *)
	
    method m_gets name params =
        match name with
	              
            (* Destructor *)
	        | "class_destroy" -> 
                  if buffer_on then print_string (Buffer.contents buffer); ref Null
            
            | "cookies" -> ignore_for_now params
	        
            (* Properties *)
	        | "buffer" -> arg0 "Response.Buffer" params;
		          wrap_bool buffer_on
    
            (* Methods *)
            | "addheader" -> ignore_for_now params
	        | "clear" -> arg0 "Response.Clear" params;
			      if buffer_on then
                      (Buffer.clear buffer; ref Null)
			      else
				      failwith "Cannot clear Response buffer when buffering is not enabled"
                          
	        | "end" -> ( arg0 "Response.End" params;
			             if buffer_on then self # print (Buffer.contents buffer);
			             exit 0 )
                  
            | "flush" -> arg0 "Response.Flush" params;
	              if buffer_on then
				      ( self#print (Buffer.contents buffer); 
				        Buffer.clear buffer;
				        ref Null )
			      else
				      failwith "Cannot flush Response buffer when buffering is not enabled"
                          
	        | "write" -> 
                  let s = get_string !(arg1 "Response.Write" params) in
                  if buffer_on then Buffer.add_string buffer s
			      else self # print s;
			      ref Null
	        
            | _ -> self # not_found name params
                  
    method m_lets name params =
        match name with
	        | "buffer" ->
                  let v = get_bool !(arg1 "Response.Buffer" params) in
                  if have_output then
				      failwith "Cannot change the value of Response.Buffer after data has been output."
			      else
                      buffer_on <- v; 
			      ref Null

            | "cookies" -> ignore_for_now params
            | "contenttype" -> 
                  content_type <- get_string !(arg1 "Response.ContentType" params);
                  ref Null

            | "cachecontrol"
            | "expires"
            | "expiresabsolute" -> ignore_for_now params

	        | _ -> self # not_found name params

					
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


end

(** {4 Request} *)

(** The request object parses the environment variables [GET] and [POST] as
	though they were CGI querystrings.  For example [hello.asp?foo=1&baz=2]
	would set [foo] to [1] and [baz] to [2].  All request data is initially
	a string. *)

(** [Request.QueryString( key )] returns the [key] variable parsed out of
	the [GET] environment variable *)
class request =
    let parse s =
        let hash = Hashtbl.create 10 in
        let ampersand = Str.regexp "&" in
        let equals = Str.regexp "=" in
        let entries = Str.split ampersand s in
        List.iter 
            (fun entry -> 
                 match Str.split equals entry with
                     | [name; value] -> Hashtbl.add hash (String.lowercase name) value
                     | _ -> raise (Runtime.Error (sprintf "Invalid query string string: '%s'" s)))
            entries;
        hash
    in
    let form =
        if try "POST" = (Sys.getenv "REQUEST_METHOD") with Not_found -> false then
            let content_length = int_of_string (Sys.getenv "CONTENT_LENGTH") in
            let post = String.create content_length in
            input stdin post 0 content_length;
            post
        else
            ""
    in
    let querystring =  (try Sys.getenv "QUERY_STRING" with Not_found -> "") in

    let maybe_find hashtbl key =
        wrap_string (try 
                         Hashtbl.find hashtbl (String.lowercase key) 
                     with 
                         | Not_found -> "")
    in

    let array_of_keys hashtbl =
        let keys =
            Hashtbl.fold
                (fun key value li ->
                     if List.mem key li then
                         li
                     else
                         key :: li)
                hashtbl
                []
        in
        wrap_array (Array.of_list (List.map wrap_string keys))
    in
    
    let hashtbl_method hashtbl params =
        match params with
            | [x] -> maybe_find hashtbl (get_string !x)
            | [] -> array_of_keys hashtbl
            | _ -> failwith "Request.Something"
    in
object(self)
    inherit opaque_object

    val querystring_hashtbl = parse querystring
    val form_hashtbl = parse form

    val all_server_variables =
        [| "ALL_HTTP";
           "ALL_RAW";
           "APPL_MD_PATH"; 
           "APPL_PHYSICAL_PATH"; 
           "AUTH_PASSWORD";
           "AUTH_TYPE";
           "AUTH_USER";
           "CERT_COOKIE";
           "CERT_FLAGS";
           "CERT_ISSUER";
           "CERT_KEYSIZE";
           "CERT_SECRETKEYSIZE";
           "CERT_SERIALNUMBER";
           "CERT_SERVER_ISSUER";
           "CERT_SERVER_SUBJECT";
           "CERT_SUBJECT";
           "CONTENT_LENGTH";
           "CONTENT_TYPE";
           "GATEWAY_INTERFACE";
           "HTTPS";
           "HTTPS_KEYSIZE";
           "HTTPS_SECRETKEYSIZE";
           "HTTPS_SERVER_ISSUER";
           "HTTPS_SERVER_SUBJECT";
           "INSTANCE_ID";
           "INSTANCE_META_PATH";
           "LOCAL_ADDR";
           "LOGON_USER";
           "PATH_INFO";
           "PATH_TRANSLATED";
           "QUERY_STRING";
           "REMOTE_ADDR";
           "REMOTE_HOST";
           "REMOTE_USER";
           "REQUEST_METHOD";
           "SCRIPT_NAME";
           "SERVER_NAME";
           "SERVER_PORT";
           "SERVER_PORT_SECURE";
           "SERVER_PROTOCOL";
           "SERVER_SOFTWARE";
           "URL";
           "HTTP_ACCEPT";
           "HTTP_ACCEPT_LANGUAGE";
           "HTTP_CONNECTION";
           "HTTP_HOST";
           "HTTP_USER_AGENT";
           "HTTP_COOKIE";
           "HTTP_ACCEPT_ENCODING";
           "HTTP_ACCEPT_CHARSET";
           "HTTP_KEEP_ALIVE" |]



    method strname = "Request"

    method private default params =
        let fieldname = String.lowercase (get_string !(arg1 "Request( )" params)) in
        ref (String 
                 (try Hashtbl.find querystring_hashtbl fieldname
                  with Not_found -> 
                      try Hashtbl.find form_hashtbl fieldname
                      with Not_found -> ""))

    method m_gets name params =
        match name with
            | "" -> self # default params

            | "cookies" -> ignore_for_now params

            | "querystring" -> hashtbl_method querystring_hashtbl params
            

            | "form" -> hashtbl_method form_hashtbl params

            | "servervariables" -> 
                  ( match params with
                        | [] -> wrap_array (Array.map wrap_string all_server_variables)
                        | [x] -> 
                              ( try wrap_string (Sys.getenv (get_string !x))
                                with Not_found -> wrap_string "" )
                        | z -> raise (Invalid_arg_count ("Request.ServerVariables", 1, (List.length z))) )

	        | _ -> self # not_found name params
            
    method m_lets name params =
        match name with
            | "cookies" -> ignore_for_now params
            | _ -> self # not_found name params

end

(** {4 Server} *)

(** The Server object is parameterized by the runtime; this is to avoid passing
	the runtime to every builtin function.  If we passed the runtime to every
	bultin function, then Eval could be implemented without being a special
	case, but a lot of craziness might ensue! (maybe this is a good thing)
	In any event, it would be a lot of editing :-) *)
class server the_runtime =
    let path_to script =
        match script.[0] with
            | '\\' | '/' -> script
            | _ -> Filename.concat (Sys.getcwd ()) script
    in
object(self)
	inherit opaque_object
	val runtime = the_runtime

    method strname = "ASP.Server"

    (** This is a way to fudge the rootdir to be the PWD if it isn't in the environment *)
    method rootdir =
        try Sys.getenv "DOCUMENT_ROOT"
        with Not_found -> 
            Sys.getcwd ()
        

	(** [Server.CreateObject("className")] is the same as [new className] *)
	method private create_object_method params =
	    match params with
	        | [x] -> wrap_object ((get_class runtime (Symbol.of_string (get_string !x))) ())
	        | z -> invalid_arg_count "Server.CreateObject" 1 z
                  
    (** [Server.MapPath("RelativePathFromScript")] 
      If the path starts with "/" or "\\" then it is a path from the server's root,
      otherwise it is a path from the current script. *)
                  
	method private map_path_method params =
		let relpath = (get_string !(arg1 "Server.MapPath" params)) in
        match relpath.[0] with
            | '\\' | '/' ->
                  wrap_string (self # rootdir ^ relpath)

            | _ ->
                  wrap_string (Filename.concat 
                                   (Filename.dirname (path_to runtime.script_path))
			                       relpath)
                  
    (** [Server.HtmlEncode str] escapes any key characters for html, for
	example changing [<] to [&gt;] and [&] to [&amp;] *)

    method m_gets name params =
        match name with

	        (* Property *)
	        | "scripttimeout" -> not_implemented params
				  
	        (* Methods *)
	        | "createobject" -> self # create_object_method params
	        | "execute" -> not_implemented params
	        | "getlastError" -> not_implemented params
	        | "htmlencode" -> 
                  let s = get_string !(arg1 "Server.HtmlEncode" params) in
                  let encoded = Nethtml.encode [(Nethtml.Data s)] in

                  wrap_string (String.concat "" 
                                   (List.map (fun doc -> match doc with
                                                  | Nethtml.Data s -> s
                                                  | _ -> "")
                                        encoded))

	        | "mappath" -> self#map_path_method params
	        | "transfer" -> not_implemented params
	        | "urlencode" -> not_implemented params
	              
	        | _ -> self # not_found name params
    
    method m_lets name =
        match name with
	        | "scripttimeout" -> not_implemented
	        | _ -> self # not_found name
                  
end
;;

(** {4 Loader Function} *)
    
let load runtime =

    (* The Session and Application are in their own module "session" because they
       are rather complex, and will have different library dependencies *)
    runtime.output <- Pervasives.print_string;
	Runtime.add_class runtime (Symbol.of_string "Response")
        (fun () -> (new response true print_string :> object_t));

	Runtime.add_class runtime (Symbol.of_string "Request")
        (fun () -> (new request :> object_t));

	Runtime.add_class runtime (Symbol.of_string "Server") (fun () -> (new server runtime :> object_t));
    
	List.iter (fun (objname, classname) -> 
				   Runtime.add_object runtime ~of_class:(Symbol.of_string classname)
                   (Symbol.of_string objname))
		[
		    (*=========== Intrinsic ASP objects ==========*)
			"Response", "response";
			"Server", "server";
	        "Request", "request";
		]

(* This module contains both the console version and a CGI variable version *)
let _ =
	(*Loader.register_module "aspConsole" load;*)
	register_module "aspIntrinsics" load
