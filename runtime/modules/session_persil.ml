
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

(** {4 Placeholder functions} *)

(** For flagging unimplemented functions *)
let ignore_for_now params = ref Null	

let not_implemented = (fun _ -> raise (Not_implemented "builtin function"))

class application = 
    let store = 
        Sfile_pstore.make_sfile_pstore 
            "/tmp/aspcc_application" 
            "aspcc_application"
    in
    let hash =
        Persist.install_store store;
        let persist_hash =
            Sfile_pstore.make_keyed_persistent 
                ~default:(Hashtbl.create 10)
                "application";
        in
        Persist.get persist_hash
    in
object(self)
    inherit opaque_object
    
    method classname = "ASP.Application"

    method m_get name params =
        match String.lowercase name with
            | "" -> 
                  let s = String.lowercase (get_string !(arg1 "Application (get)" params)) in
                  try wrap_string (Hashtbl.find hash (String.lowercase s))
                  with Not_found -> wrap_string ""
                      
            | _ -> self # not_found name params

    method m_let name params =
        match String.lowercase name with
            | "" ->
                  let key, value = arg2 "Application (let)" params in
                  Hashtbl.replace hash 
                      (String.lowercase (get_string !key))
                      (get_string !value);
                  ref Null
            
            | _ -> self # not_found name params
                  
end

class session ?session_cookie = 
(*    let perlhash =
        perl
            match session_cookie with
                | Some c -> "tie %session, 'Apache::Session::File', " ^ session_cookie 
                | None -> "tie %session, 'Apache::Session::File', undef"
    in *)
object(self)
    inherit opaque_object

    method classname = "ASP.Session"
(*
    method m_get name params =
        match String.lowercase name with
            | "" -> 
                  let s = String.lowercase (get_string !(arg1 "Session" params)) in
                  
            

    method m_let name = ignore_for_now
*)
end

(** {4 Loader Function} *)
    
let load runtime =
    runtime.output <- Pervasives.print_string;
    
    ignore (Perl.eval "use Apache::Session::File");

    Runtime.add_class runtime "Application" (fun () -> (new application :> object_t));
    (*Runtime.add_class runtime "Session" (fun () -> (new session :> object_t));*)

	List.iter (fun (objname, classname) -> 
				   Runtime.add_object runtime ~of_class:classname objname)
		[
		    (*=========== Intrinsic ASP objects ==========*)
	        "Application", "application";
			(*"Session", "session"*)
		]

(* This module contains both the console version and a CGI variable version *)
let _ =
	(*Loader.register_module "aspConsole" load;*)
	register_module "session" load
