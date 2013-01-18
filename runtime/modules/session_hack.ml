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

open UnixLabels

(* NOT SECURE, DONT USE *)
class persist_hack classname filename = 
    let array_of_keys hashtbl =
        Array.of_list (Hashtbl.fold
                           (fun key value li ->
                                if List.mem key li then
                                    li
                                else
                                    key :: li)
                           hashtbl
                           [])
    in
object(self)
    inherit opaque_object

    val hash : (string, string) Hashtbl.t = Hashtbl.create 10
                                                
    initializer (
        if Sys.file_exists filename then
            (let chan = open_in filename in
            let file = descr_of_in_channel chan in
            (*lockf file ~mode:F_RLOCK ~len:0;*)
            while
                try
                    let s = input_line chan in
                    Scanf.sscanf s "%S\t%S" (fun key value -> Hashtbl.add hash key value);
                    true
                with
                    | End_of_file -> false
                    | Scanf.Scan_failure _ -> true (* We just ignore corrupted data LOLLLOREZZ *)
                          
            do () done);
        
        at_exit (fun () -> self # save)
    )

    method strname = classname

    method get s =
        try Hashtbl.find hash (String.lowercase s)
        with Not_found -> ""

    method set s v =
        Hashtbl.replace hash (String.lowercase s) v

    method keys =
        array_of_keys hash
            
    method save =
        let chan = open_out filename in
        let file = descr_of_out_channel chan in
        (*lockf file ~mode:F_LOCK ~len:0;*)
        Hashtbl.iter 
            (fun key value -> fprintf chan "%S\t%S\n" key value)
            hash;
        
        close_out chan

    method m_gets name params =
        match name with
            | ""
            | "item" ->
                  (match params with
                      | [] -> wrap_array (Array.map wrap_string (self # keys))
                      | [s] ->
                            let key = (get_string !s) in
                            wrap_string (self # get key)
                      | z -> raise (Invalid_arg_count ( (self # strname ^ ".Item"), 
                                                        1, 
                                                        List.length z)))
            
            | "removeall" ->
                  arg0 (self # strname ^ ".RemoveAll") params;
                  Hashtbl.clear hash;
                  ref Null

            | _ -> self # not_found name params

    method m_lets name params =
        match name with
            | "" 
            | "item" ->
                  let key, value = arg2 (self # strname ^ ".item") params in
                  self # set
                      (get_string !key)
                      (get_string !value);
                  ref Null
                      
            | _ -> self # not_found name params
end

class application =
object(self)
    inherit opaque_object
    val persist = (new persist_hack 
                       "ASP.Application.Contents" 
                       "/tmp/aspcc_application")
        
    method strname = "ASP.Application"
                           
    method m_gets name params =
        match name with
            | ""
            | "contents" ->
                  arg0 "Application.Contents" params;
                  wrap_object (persist :> object_t)
                      
            | _ -> self # not_found name params
    
    method m_lets name params =
        match name with
            | ""
            | "contents" -> persist # m_lets "item" params
            | _ -> self # not_found name params
end

class session ?session_cookie use_cgi = 
    let sessionfile_name id =
        sprintf "/tmp/aspcc_session_%i" id
    in
    
    let rec find_new_id () =
        let id = Random.int max_int in
        if Sys.file_exists (sessionfile_name id)
        then find_new_id ()
        else id
    in
    
    let sessionid, filename =
        if use_cgi then
            match session_cookie with
                | Some _
                | None -> 
                      let id = find_new_id () in
                      id, sessionfile_name id
        else
            try
                0, (Sys.getenv "HOME") ^ "/.aspcc_session"
            with
                | Not_found -> failwith "Not in CGI mode, but also no HOME variable."
    in
object(self)
    inherit opaque_object
        
    val persist = (new persist_hack 
                       "ASP.Session.Contents"
                       filename)
                      
    method strname = "ASP.Session"

    method m_gets name params =
        match name with
            | ""
            | "contents" ->
                  ( match params with
                        | [] -> wrap_object (persist :> object_t)
                        | [x] -> persist # m_gets "item" params
                        | li -> self # invalid_arg_count name 1 li )
            
            | "abandon" -> persist # m_gets "removeall" params

            | "sessionid" -> wrap_int sessionid
                      
            | _ -> self # not_found name params

    method m_lets name params =
        match name with
            | ""
            | "contents" -> persist # m_lets "item" params

            | _ -> self # not_found name params
end

(** {4 Loader Function} *)
    
let load runtime =
    Runtime.add_class runtime (Symbol.of_string "Application")
        (fun () -> (new application :> object_t));

    Runtime.add_class runtime (Symbol.of_string "Session")
        (fun () -> (new session (Runtime.get_option runtime `use_cgi) :> object_t));

	List.iter (fun (objname, classname) -> 
			       (Runtime.add_object 
                        runtime
                        ~of_class:(Symbol.of_string classname)
                        (Symbol.of_string objname)))
		[
		    (*=========== Intrinsic ASP objects ==========*)
	        "application", "application";
			"session", "session"
		]

let _ =
	register_module "session_hack" load
