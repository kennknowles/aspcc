
(** ASPCC module with the Tools object *)

open VbValues
open VbClass
open VbTypes
open Runtime
open Printf

(* Just a hack *)
class browsertype =
    let browser, version =
        try
            let useragent = Sys.getenv "HTTP_USER_AGENT" in
            if (String.sub useragent 0 7) = "Mozilla" then
                "Mozilla", String.sub useragent 8 3
            else if (String.sub useragent 0 2) = "IE" then
                "IE", String.sub useragent 3 3
            else
                "Mozilla", "6.0"
        with
            | Not_found -> "Mozilla", "6.0"
    in
object(self)
	inherit opaque_object
    method strname = "MSWC.BrowserType"

    method m_gets name params =
        match name with
            | "browser" -> wrap_string browser
            | "version" -> wrap_string version
            | _ -> self # not_found name params
end
;;

(** {4 Loader Function} *)

let load runtime =
    Runtime.add_class runtime 
        (Symbol.of_string "MSWC.BrowserType")
        (fun () -> (new browsertype :> object_t));
    
    Runtime.add_class runtime 
        (Symbol.of_string "BrowserType")
        (fun () -> (new browsertype :> object_t));
;;

let _ =
	register_module "browsercap" load
