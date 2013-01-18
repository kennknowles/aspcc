
(* Printing functions *)
open AspAst
open Printf

let trim_eol s =
	try
		ignore (String.index s '\n');
		String.sub s 0 ((String.length s) - 1)
	with
		| Not_found -> s
		      (* Invalid_argument means the string has 0 length *)
		| Invalid_argument _ -> s

let indent indentation str =
	(Str.global_replace (Str.regexp "^") indentation (trim_eol str)) ^ "\n"

let concat_map ?(delim = "\n") f li =
    String.concat delim (List.map f li)

(*===========================================================================*)
(* XML helpers                                                               *)
(*===========================================================================*)

let surround tagname contents = sprintf "<%s>%s</%s>\n" tagname contents tagname
let surround_indent tagname contents =
	sprintf "<%s>\n%s</%s>\n" tagname (indent "\t" contents) tagname

let surround_concat tagname li =
	String.concat "" (List.map (surround_indent tagname) li)

(*===========================================================================*)
(* For outputing the AST aafter parsing *)
(*===========================================================================*)

let rec sprint_id id =
	match id with
	    | AspAst.AtomicId name -> surround "AtomicId" (Symbol.to_string name)
	    | AspAst.Dot (base,member) ->
              (surround_indent 
                   "Dot"
			       ( (surround_indent "Base" (sprint_id base)) ^
				     (surround "Member" (Symbol.to_string member))))

	    | AspAst.Indices (base,indices) ->
		      "<Indices>\n"^
			  (indent "\t" (
				   "<Base>\n" ^
				   (indent "\t" (sprint_id base)) ^
				   "</Base>\n" ^
				   "<Vals>\n" ^
				   (indent "\t" (String.concat ""
						             (List.map sprint_rvalue indices))) ^
				   "</Vals>\n")) ^
		      "</Indices>\n"

and sprint_rvalue rval =
    try
	    (match rval with
	         | AspAst.Int (_, i) -> sprintf "<Int>%li</Int>\n" i
	         | AspAst.Float f -> sprintf "<Float>%f</Float>\n" f
	         | AspAst.String s -> sprintf "<String>%s</String>\n" s 
	         | AspAst.Identifier i ->
		           "<Identifier>\n" ^ 
			       (indent "\t" (sprint_id i)) ^
		           "</Identifier>\n"

	    (* redo when un-lazy 
	       | AspAst.Equals (v1, v2) ->
		   "<Equals>\n" ^
		   (indent "\t" ((sprint_rvalue v1) ^ (sprint_rvalue v2))) ^
		   "</Equals>\n"

	       | AspAst.Concat (v1, v2) ->
		   "<Concat>\n" ^
		   (indent "\t" ((sprint_rvalue v1) ^ (sprint_rvalue v2))) ^
		   "</Concat>\n"

           | AspAst.New o -> "<New>\n" ^
           (indent "\t" (sprint_id o)) ^
           "</Concat>\n"

           | AspAst.Is (v1, v2) ->
           "<Is>\n" ^
           (indent "\t" ((sprint_rvalue v1) ^ (sprint_rvalue v2))) ^
           "</Is>\n"

           | AspAst.GreaterEqual (v1, v2) ->
           "<GreaterEqual>\n" ^
           (indent "\t" ((sprint_rvalue v1) ^ (sprint_rvalue v2))) ^
           "</GreaterEqual>\n"

           | AspAst.LessEqual (v1, v2) ->
           "<LessEqual>\n" ^
           (indent "\t" ((sprint_rvalue v1) ^ (sprint_rvalue v2))) ^
           "</LessEqual>\n"

           | AspAst.Greater (v1, v2) ->
           "<Greater>\n" ^
           (indent "\t" ((sprint_rvalue v1) ^ (sprint_rvalue v2))) ^
           "</Greater>\n"

           | AspAst.Less (v1, v2) ->
           "<Less>\n" ^
           (indent "\t" ((sprint_rvalue v1) ^ (sprint_rvalue v2))) ^
           "</Less>\n"

           | AspAst.NotEquals (v1, v2) ->
           "<NotEquals>\n" ^
           (indent "\t" ((sprint_rvalue v1) ^ (sprint_rvalue v2))) ^
           "</NotEquals>\n" *)
	    )
    with Match_failure _ -> "<UnknownRValue />\n"
        
(* | _ -> "<UnknownRValue />\n" *) (* these guards are commented out
                                      to make it easier to see which cases
                                      remain unimplemented, and to
                                      remind about their absence *)

let sprint_param (byval,name) =
    Symbol.to_string name

let rec sprint_statement (line_num, statement) =
    try 
        (match statement with
             | AspAst.EmptyStatement -> "<EmptyStatement />\n"
             | AspAst.Assignment (a_id, a_val) -> 
	               "<Assignment>:\n" ^
	               (indent "\t"
	                    "<Id>\n" ^
	                (indent "\t" (sprint_id a_id)) ^
	                "</LValue>\n" ^
	                "<RValue>\n" ^
	                (indent "\t" (sprint_rvalue a_val)) ^
	                "</RValue>\n") ^
	               "</Assignment>\n"

             | AspAst.Call func_app ->
	               "<Call>\n" ^
	               (indent "\t" (sprint_id func_app)) ^
	               "</Call>\n"

             | AspAst.Class (name, statements) ->
                   ( surround_indent "Class"
	                     ( (surround "Name" (Symbol.to_string name)) ^
	                       (surround_indent "Definition"
	                            (concat_map ~delim:"" sprint_statement statements))))

             | AspAst.Comment c -> "<Comment>" ^ (trim_eol c) ^ "</Comment>\n" 

             | AspAst.CompoundStatement (compound_type, statements) ->
	               (match compound_type with
                        | AspAst.For (var, start, finish, inc) ->
	                          ( surround_indent "For"
		                            ( (surround "Var" (Symbol.to_string var)) ^
		                              
                                      (surround_indent "Start"
		                                   (sprint_rvalue start)) ^
                                      
		                              (surround_indent "Finish"
		                                   (sprint_rvalue start))))
		           
                              
                        | AspAst.ForEach (name, collection) ->
                              ( surround_indent "ForEach"
                                    ( (surround "Var" (Symbol.to_string name)) ^
		                              (surround_indent "Collection"
		                                   (sprint_rvalue collection)) ) )
                              
                        | AspAst.While expr ->
	                          "<While>\n" ^
	                          (indent "\t" (
		                           "<Condition>\n" ^
		                           (indent "\t" (sprint_rvalue expr)) ^
		                           "</Condition>\n"))
	                    | AspAst.With lval ->
	                          "<With>\n" ^
	                          (indent "\t" (
		                           "<Id>\n" ^
		                           (indent "\t" (sprint_id lval)) ^
		                           "</Id>\n"))

	                    | AspAst.DoWhile _ -> ""
                   ) ^ 
	    (indent "\t" (
	                    "<Body>\n" ^
	                    (indent "\t"
	                         (String.concat "" (List.map sprint_statement statements))) ^
	                    "</Body>\n")) ^

	               (match compound_type with
	                    | AspAst.For _ -> "</For>\n"
	                    | AspAst.ForEach _ -> "</ForEach>\n"
	                    | AspAst.While _ -> "</While>\n"
	                    | AspAst.With _ -> "</With>\n"
	                    | AspAst.DoWhile _ -> "")

        
             | AspAst.Const (name, rval) -> 
                   ( surround_indent "Const"
	                     ( (surround "Name" (Symbol.to_string name)) ^
	                       (surround_indent "Value"
	                            (sprint_rvalue rval))))
	               
             | AspAst.Dim id_list -> 
	               "<Dim>\n" ^
	               (indent "\t" (String.concat "," (List.map sprint_id id_list))) ^
	               "</Dim>\n"

             | AspAst.Exit exit_target ->
                   sprintf "<Exit Target=\"%s\">" (match exit_target with
                                                       | `Function -> "Function"
                                                       | `While -> "While"
                                                       | `Sub -> "Sub"
                                                       | `Property -> "Property"
                                                       | `With -> "With"
                                                       | `Do -> "Do"
                                                       | `For -> "For" )
	               
             | AspAst.Function (name, params, body) ->
		           ( surround_indent "Function"
		                 ( (surround "Name" (Symbol.to_string name)) ^
		                   (surround_concat "Param" (List.map sprint_param params)) ^
		                   (surround_indent "Body"
			                   (concat_map sprint_statement body))))
	               
             | AspAst.Html html -> "<Html>" ^ html ^ "</Html>\n"
	               
             | If (rval, then_branch, else_branch) ->
                   surround_indent "If" (
                       (sprint_rvalue rval) ^
                       (surround_indent "Then" (concat_map sprint_statement then_branch)) ^
                       (surround_indent "Else" (concat_map sprint_statement else_branch)))
                   

             | AspAst.InlineExpr e -> 
	               "<InlineExpr>\n" ^
	               (indent "\t" (sprint_rvalue e)) ^
	               "</InlineExpr>\n"
	               

             | AspAst.MemberDef (access,default,statement) -> ();
                   surround_indent
	                   (match access with
	                        | `Private -> "Private"
	                        | `Public -> "Public")
                       
                       ( (if default then "<Default />\n" else "") ^
	                     (sprint_memberstatement statement))
                   

             | AspAst.ReDim (preserve, id_list) ->
	               "<ReDim>\n" ^
	               (indent "\t" (
	                    (if preserve then "<Preserve />" else "") ^
	                    (String.concat "" (List.map sprint_id id_list)))) ^
	               "</Redim>\n"
                   
             | AspAst.Set (a_id, a_val) -> 
	               "<Set>:\n" ^
	               (indent "\t"
	                    "<Id>\n" ^
	                (indent "\t" (sprint_id a_id)) ^
	                "</LValue>\n" ^
	                "<RValue>\n" ^
	                (indent "\t" (sprint_rvalue a_val)) ^
	                "</RValue>\n") ^
	               "</Set>\n"
	         
             | AspAst.Ssi (kind, filename) ->
		           sprintf "<Ssi Kind=\"%s\" FileName=\"%s\" />\n" kind filename


             | AspAst.SubCall (id, vals) ->
	               "<SubCall>\n" ^
	               (indent "\t" 
	                    ((sprint_id id) ^
	                     "<Args>\n"^
	                     (indent "\t" (String.concat "" 
			                               (List.map sprint_rvalue vals))) ^
	                     "</Args>\n")) ^
	               "</SubCall>\n")
    with Match_failure _ -> "<UnknownStatement />\n"

(*	| _ -> "<UnknownStatement />\n" *)

and sprint_memberstatement statement =
    try
	    (match statement with
	         | AspAst.MemberIdent id_list ->
		           "<Field>\n" ^
			       (indent "\t" (String.concat "," (List.map sprint_id id_list))) ^
		           "</Field>\n"
	         | AspAst.PropertyGet (name,params,statements) ->
		           "<PropertyGet>\n" ^
			       (indent "\t" (
				        "<Args.../>\n" ^
				        "<Body>\n" ^	
					    (indent "\t" (String.concat "" 
						                  (List.map sprint_statement statements))) ^
				        "</Body>")) ^
		           "</PropertyGet>"
	         | AspAst.MemberFunction (name,params,statements) ->
		           "<MemberFunction>\n" ^
			       (indent "\t" (
				        "<Args.../>\n" ^
				        "<Body>\n" ^	
					    (indent "\t" (String.concat "" 
						                  (List.map sprint_statement statements))) ^
				        "</Body>")) ^
		           "</MemberFunction>")
    with Match_failure _ -> "<UnknownMember />\n"
        (*	| _ -> "<UnknownMember />\n" *)

let sprint_page page =
	"<Page>\n"
	^ (indent "\t" 
		   (String.concat
			    ""
			    (List.map sprint_statement page)))
	^ "</Page>\n"

let print_page page =
	print_string (sprint_page page)
