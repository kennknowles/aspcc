
(** ASPCC module with the Tools object *)

open VbValues
open VbClass
open VbTypes
open Runtime
open Printf

class tools =
object(self)

	inherit opaque_object

    method strname = "MSWC.Tools"

    method m_gets name params =
        match name with
            | "fileexists" -> 
                  wrap_bool (Sys.file_exists (get_string !(arg1 "Tools.FileExists" params)))
                  
            | _ -> self # not_found name params
end
;;

(** {4 Loader Function} *)

let load runtime =
    Runtime.add_class runtime 
        (Symbol.of_string "MSWC.Tools")
        (fun () -> (new tools :> object_t));
    
    Runtime.add_class runtime 
        (Symbol.of_string "Tools")
        (fun () -> (new tools :> object_t));
;;

let _ =
	register_module "tools" load
