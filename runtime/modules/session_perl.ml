
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
    let my_hash =
        ignore (Perl.eval "tie %application, 'Apache::Session::File', 1");
        Perl.get_hv "application"
    in
object(self)
    inherit opaque_object
    
    val mutable is_init = false

    method classname = "ASP.Application"

    method m_get name params =
        match String.lowercase name with
            | "" -> 
                  let s = String.lowercase (get_string !(arg1 "Application (get)" params)) in
                  wrap_string (Perl.string_of_sv (Perl.hv_get my_hash s))

            | _ -> self # not_found name params

    method m_let name params =
        match String.lowercase name with
            | "" ->
                  let key, value = arg2 "Application (let)" params in
                  Perl.hv_set 
                      my_hash 
                      (String.lowercase (get_string !key))
                      (Perl.sv_of_string (get_string !value));
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
