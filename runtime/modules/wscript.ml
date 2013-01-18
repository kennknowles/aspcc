
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
open Printf

class wsh_arguments argv =
object(self)
    inherit collection "WshArguments" (Array.map wrap_string argv)
end

class wsh_environment =
object(self)
    inherit opaque_object
    method strname = "WshEnvironment"
    
    val environment = Unix.environment ()

    method m_gets name params =
        match name with
            | ""
            | "item" -> wrap_string (Sys.getenv (get_string !(arg1 "WshEnvironment.Item" params)))
                  
            | "count"
            | "length" -> wrap_int (Array.length environment)
                  
            | _ -> self # not_found name params
end

class wsh_shell =
object(self)
    inherit opaque_object
    method strname = "WshShell"
                           
    method m_gets name params =
        match name with
            | "environment" -> wrap_object (new wsh_environment :> object_t)

            (* Note: this doesn't have the toggle of blocking/nonblocking that it should *)
            | "run" -> wrap_int (Sys.command (get_string !(arg1 "WshShell.Run" params)))

            | _ -> self # not_found name params
end

class wscript the_runtime =
object(self)
	inherit opaque_object
    method strname = "WScript"

    method m_gets name params =
        match name with
            | "arguments" -> 
                  let len = Array.length Sys.argv in
                  arg0 "WScript.Arguments" params;
                  (* TODO: vary the number increment based on whether it is #! or not or something *)
                  wrap_object (new wsh_arguments (Array.sub Sys.argv 2 (len - 2)) :> object_t)

            | "createobject" ->
                  let classname = Symbol.of_string (get_string !(arg1 "WScript.CreateObject" params)) in
                  wrap_object ((get_class the_runtime classname) ())

            | "echo" ->
                  let s = get_string !(arg1 "WScript.Echo" params) in
                  the_runtime.output (s ^ "\n");
                  ref Null

            | "scriptfullname" -> wrap_string the_runtime.script_path
            | "scriptname" -> wrap_string (Filename.basename the_runtime.script_path)

            | "quit" ->
                  let i = get_int !(arg1 "WScript.Quit" params) in
                  exit i

	        | _ -> self # not_found name params
end
;;

(** {4 Loader Function} *)
    
let load runtime =
	Runtime.add_class runtime (Symbol.of_string "WScript")
        (fun () -> (new wscript runtime :> object_t));
    
	Runtime.add_class runtime (Symbol.of_string "WScript.Shell")
        (fun () -> (new wsh_shell :> object_t));

	Runtime.add_class runtime (Symbol.of_string "WScript.Arguments")
        (fun () -> (new wsh_arguments Sys.argv :> object_t));
    
    Runtime.add_object runtime ~of_class:(Symbol.of_string "WScript")
        (Symbol.of_string "wscript")

let _ =
	register_module "wscript" load
