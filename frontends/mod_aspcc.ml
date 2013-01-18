
(** Mod_caml based apache module to interpret ASP/VbScript pages in a 100%
	compatible way, including intrinsic objects and ADO *)



(** The "real" intrinsic ASP objects are defined here, so that they can
	interface with the apache API provided by Mod_caml *)

open Apache
open VbTools
open VbTypes
open Run

(*let scope = Run.create_runtime ();;*)

let runtime = Run.create ()
;;

let _ =
	List.iter
		(fun x -> Run.Loader.apply_module runtime x)
		[]
;;

let invalid_arg_count funcname num li =
    raise (Invalid_arg_count (funcname, num, List.length li))

(** {4 Response Object} *)

exception Response_end

(** The class of response objects *)
class response request = 
object (self)
	val request = request 

	(** Flag to indicate whether any output has been sent to the client *)
	val mutable have_output = false

	(** If [have_output] is true, then it is illegal to change this *)
	val mutable buffer_on = false

	(** Internal buffer *)
	val buffer = Buffer.create 80

	
	(*=================== properties =======================*)

	(** [Response.buffer] indicates whether output will be buffered *)
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

	(** [Response.end] terminates the response, flushing all data to the
		client *)
	method private end_method = function params ->
		match params with
		| [] -> 
			if buffer_on then print_string request (Buffer.contents buffer);
			raise Response_end
		| z -> invalid_arg_count "Response.End" 0 z

	(** [Response.flush] outputs everything in the buffered response *)
	method private flush_method = function params ->
		match params with
		| [] ->
			if buffer_on then
				(print_string request (Buffer.contents buffer); 
				Buffer.clear buffer;
				ref Null)
			else
				raise (Failure ("Cannot flush Response buffer when buffering " ^
								"is not enabled"))
		| z -> invalid_arg_count "Response.Clear" 0 z

	(** [Response.Write( str )] writes [str] to the client request *)
	method private write_method = function params ->
		match params with
		| [x] -> 
			have_output <- true;
			if buffer_on then
				Buffer.add_string buffer (get_string !x)
			else
				print_string request (get_string !x);
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
		if buffer_on then print_string request (Buffer.contents buffer);
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
class response_class request = object
	val request = request
	method create_object () =
		let x = (new response request) in (x :> object_t)
end



(** {4 Apache interface} *)

let handle_page request filename =
	let infile = Unix.in_channel_of_descr (Unix.openfile filename [Unix.O_RDONLY] 0) in

	let lexbuf = Lexing.from_channel infile in
	try
		let ast = AspParser.page AspLexer.token lexbuf in
		let my_runtime = Run.copy runtime in

		(*	create a local runtime so that we can point Response and Request to the
			proper places *)
		Run.set_html_func my_runtime (print_string request);

		Run.add_opaque_class my_runtime "Response" (new response_class request);
		Run.add_builtin_object my_runtime "Response" "response";

		(try
			Run.page my_runtime ast
		with
			Response_end  -> ())
	with
		Parsing.Parse_error -> print_string request ("<table><tr><td><pre>" ^ (Asp.get_errors ()) ^ "</pre></td></tr></table>")
	

(** The request handler; it checks the filename for .asp and declines if it
	doesn't match *)
let handler request =
	match Request.filename request with
	| None -> print_string request "No filename supplied."; DONE (*failwith "No filename supplied"*)
	| Some s -> 
		if not (try 
					(String.lowercase (String.sub s ((String.length s) - 4) 4)) = ".asp" 
				with
					_ -> false)
		then
			DECLINED
		else 
			(handle_page request s; DONE)
	

let _ =
	Mod_caml.register_handler handler "handler"
