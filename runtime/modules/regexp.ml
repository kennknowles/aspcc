
(** ASPCC module with the Tools object *)

open VbValues
open VbClass
open VbTypes
open Runtime
open Printf

class reg_match substrings =
object(self)
    inherit opaque_object
    method strname = "Match"
    
    val submatches = Array.map wrap_string (Pcre.get_substrings ~full_match:false substrings)

    method m_gets name params =
        match name with
            | "submatches" ->
                  ( match params with
                        | [] -> wrap_object (new collection "Submatches" submatches :> object_t)
                        | [x] -> submatches.(get_int !x) 
                        | _ -> raise (Invalid_arg_list "RegExp.Submatches") )

            | "firstindex" ->
                  arg0 "Match.FirstIndex" params;
                  wrap_int (fst (Pcre.get_substring_ofs substrings 0))

            | "length" ->
                  arg0 "Match.Length" params;
                  let start, finish = Pcre.get_substring_ofs substrings 0 in
                  wrap_int (finish - start + 1)

            | "value" ->
                  arg0 "Match.Value" params;
                  wrap_string (Pcre.get_substring substrings 0)

            | _ -> self # not_found name params

end

class regexp =
object(self)
	inherit opaque_object

    method strname = "Regexp"

    val mutable global = false
    val mutable ignore_case = false

    (** The pattern seems to be almost exactly PCRE *)
    val mutable pattern = ""
    
    method rex =
        Pcre.regexp
            ~flags:(if ignore_case then [`CASELESS] else [])
            pattern

    (* TODO: enforce the length of arg lists *)
    method m_gets name params =
        match name with
                
            (* Properties *)
            | "global" -> wrap_bool global
            | "ignorecase" -> wrap_bool ignore_case
            | "pattern" -> wrap_string pattern
        
            (* Methods *)
            | "test" -> wrap_bool (Pcre.pmatch ~rex:(self # rex)
                                       (get_string !(arg1 "Regexp.Test" params)))

            | "replace" -> 
                  let s = get_string !(arg1 "Regexp.Replace" params) in
                  wrap_string (
                      if global then
                          Pcre.replace ~rex:(self # rex) s
                      else
                          Pcre.replace_first ~rex:(self # rex) s
                  )
                      
            | "execute" -> 
                  let s = get_string !(arg1 "Regexp.Execute" params) in
                  let substrings = Pcre.exec_all ~rex:(self # rex) s in
                  wrap_object (new collection "Matches"
                                   (Array.map (fun s -> 
                                                   wrap_object (new reg_match s :> object_t))
                                        substrings) :> object_t)
                      
    method m_lets name params =
        (match name with
            | "global" -> global <- get_bool !(arg1 "Regexp.Global" params)
            | "ignorecase" -> ignore_case <- get_bool !(arg1 "Regexp.IgnoreCase" params)
            | "pattern" -> pattern <- get_string !(arg1 "Regexp.Pattern" params));
        ref Null
end
;;

(** {4 Loader Function} *)

let load runtime =
    Runtime.add_class runtime 
        (Symbol.of_string "Regexp")
        (fun () -> (new regexp :> object_t));
;;    

let _ =
	register_module "regexp" load
