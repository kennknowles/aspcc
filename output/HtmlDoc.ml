
type prop_type = Get | Set | Let

exception Will_not_overwrite_non_directory

let parent_dirname dirname =
    try
        Str.first_chars dirname (String.rindex dirname '/')
    with
        | Not_found -> ""

let rec ensure_dir dir =
    if dir <> "" then (
        ensure_dir (parent_dirname dir);
        if Sys.file_exists dir then
            try ignore (Sys.readdir dir);
            with Sys_error _ -> raise Will_not_overwrite_non_directory
        else
            Unix.mkdir dir 0o766
    )


let third (one,two,three) = three

let id x = x
let chomp s = String.sub s 0 ((String.length s) - 1)
let chomp_char c s =
	try
		ignore (String.index s c);
		chomp s
	with
		Not_found -> s

let chomp_eol s = chomp_char '\r' (chomp_char '\n' s) 

let html_option_render ?(prefix = "") ?(suffix = "") ?(separator = "") ~callback items =
    if List.length items > 0 then
        (prefix ^ (String.concat separator (List.map callback items)) ^ suffix)
    else
        ""
        
open Printf
open Doc

let html_var_doc ?(access = "") ?(prefix = "Dim") vardoc =
    sprintf "<code><span class='%s'>%s</span><span class='keyword'>%s</span> %s</code>%s" 
        access access
        prefix
        (Symbol.to_string vardoc.var_name)
        (if vardoc.var_comment = "" then "" else
             " <span class='comment'> ' " ^ (chomp_eol vardoc.var_comment) ^ "</span>")

let html_func_doc ?(access = "") ?(prefix = "Function") funcdoc =
    sprintf 
        "
<pre><code><span class='%s'>%s</span><span class='keyword'>%s</span> %s( %s ) %s %s</code> %s </pre>"
        access access
        prefix
        (Symbol.to_string funcdoc.func_name)
        (html_option_render 
             ~separator:", "
             ~callback:(fun var -> Symbol.to_string var.var_name)
             funcdoc.func_params)

        (html_option_render
             ~prefix:"\n    "
             ~separator:"\n    "
             ~callback:(html_var_doc ~prefix:"")
             (List.filter (fun var -> var.var_comment <> "") funcdoc.func_params))
        
        (if funcdoc.func_return = "" then "" else
             "\n    <span class='keyword'>Returns:</span> " ^ funcdoc.func_return)
        
        (if (chomp_eol funcdoc.func_comment) = "" then "" else "\n\n" ^ (chomp_eol funcdoc.func_comment))

        
let html_property_doc (access, default, propdoc) =
    let access_str = 
        (match access with
             | `Public -> "Public "
             | `Private -> "Private ")
    in
    sprintf "
<code><span class='%s'>%s</span><span class='keyword'>%sProperty</span> %s</code>
<table>
    <tr>
        <td>&nbsp; &nbsp; &nbsp;</td>
        <td>%s%s%s</td>
    </tr>
</table>
" 
        access_str access_str
        (if default then "Default " else "") (* Maybe functions can be default... *)
        (Symbol.to_string propdoc.prop_name)

        (match propdoc.prop_get with
             | None -> ""
             | Some f -> html_func_doc ~prefix:"Get" {f with func_name = Symbol.empty})
        (match propdoc.prop_let with
             | None -> ""
             | Some f -> html_func_doc ~prefix:"Let" {f with func_name = Symbol.empty})
        (match propdoc.prop_set with
             | None -> ""
             | Some f -> html_func_doc ~prefix:"Set" {f with func_name = Symbol.empty})

let is_public2 (access, _) = access = `Public
let is_public3 (access, _, _) = access = `Public
let is_private2 (access, _) = access = `Private
let is_private3 (access, _, _) = access = `Private

let html_class_doc classdoc =
    sprintf "
<pre><code><span class='keyword'>Class</span> %s</code>
%s</pre>
<!-- Public Table -->
<table>
    <tr>
        <td>&nbsp; &nbsp; &nbsp;</td>
        <td><pre>%s%s%s%s</pre></td>
    </tr>
</table>

<!-- Private Table -->
<table>
    <tr>
        <td>&nbsp; &nbsp; &nbsp;</td>
        <td><pre>%s%s%s%s</pre></td>
    </tr>
</table>
"
        (Symbol.to_string classdoc.class_name)
        classdoc.class_comment
        
        (* Public Vars *)
        (html_option_render 
             ~separator:"\n" 
             ~callback:(html_var_doc ~access:"Public " ~prefix:"")
             (List.map snd (List.filter is_public2 classdoc.class_vars)))

        (* Public functions / Subs / Properties *)
        (html_option_render 
             ~callback:(html_func_doc ~access:"Public " ~prefix:"Function")
             (List.map third (List.filter is_public3 classdoc.class_functions)))

        (html_option_render 
             ~callback:(html_func_doc ~access:"Public " ~prefix:"Sub")
             (List.map third (List.filter is_public3 classdoc.class_subs)))
        
        (html_option_render ~callback:html_property_doc 
             (List.filter is_public3 classdoc.class_props))
             
        (* Private Vars *)
        (html_option_render 
             ~separator:"\n" 
             ~callback:(html_var_doc ~access:"Private " ~prefix:"")
             (List.map snd (List.filter is_private2 classdoc.class_vars)))

        (* Public functions / Subs / Properties *)
        (html_option_render 
             ~callback:(html_func_doc ~access:"Private " ~prefix:"Function")
             (List.map third (List.filter is_private3 classdoc.class_functions)))

        (html_option_render 
             ~callback:(html_func_doc ~access:"Private " ~prefix:"Sub")
             (List.map third (List.filter is_private3 classdoc.class_subs)))
        
        (html_option_render ~callback:html_property_doc
             (List.filter is_private3 classdoc.class_props))

let html_include includefile =
    sprintf "<a href='%s.html'>%s</a>" includefile includefile

let html_file_doc filedoc =
    sprintf "
<html>
<style>
<!--
pre { font-family : Times }
.comment { color : green }
.keyword { color : orange }
.Public { color : blue }
.Private { color : red }
-->
</style>
<body>

<center><h2>File <span style='color: blue'>%s</span></h2></center>
<p>%s</p>

<!-- Dependencies -->
%s

<!-- Includes -->
%s

<hr />

<!-- Global Variables -->
%s
<hr />

<!-- Functions -->
%s

<!-- Subs -->
%s

<!-- Classes -->
%s

</body>
</html>
"
        filedoc.file_name
        filedoc.file_comment
        (html_option_render 
             ~prefix:"<h4>Depends upon:</h4>\n<ul>\n<li>" 
             ~suffix:"\n</li></ul></p>"
             ~separator:"</li>\n<li>" 
             ~callback:html_include
             filedoc.file_depends)
        
        (html_option_render 
             ~prefix:"<h4>Includes:</h4>\n<ul>\n<li><code>"
             ~suffix:"\n</code></li></ul></p>"
             ~separator:"</code></li>\n<li><code>"
             ~callback:html_include
             filedoc.file_includes)
        
        (html_option_render 
             ~prefix:"<pre>" ~suffix:"</pre>"
             ~separator:"\n\n" ~callback:html_var_doc filedoc.file_vars)

        (html_option_render ~callback:html_func_doc filedoc.file_functions)
        (html_option_render ~separator:"<p />" ~callback:(html_func_doc ~prefix:"Sub") filedoc.file_subs)
        (html_option_render ~callback:html_class_doc filedoc.file_classes)

let open_and_output_file_doc ?outputfile filedoc =
    let outputfile = match outputfile with
        | Some f -> f
        | None -> filedoc.file_name ^ ".html"
    in
    ensure_dir (Filename.dirname outputfile);
    
    let outchan = open_out outputfile in
    output_string outchan (html_file_doc filedoc);
    close_out outchan
    (*printf "I want to put the docs for %s in %s\n" filedoc.file_name outputfile *)

let compare_case_fold s1 s2 =
    compare (String.uppercase s1) (String.uppercase s2)


let output_main_index filename filedocs =
    let chan = open_out filename in
    fprintf chan "
        <style>
        <!--
        a { text-decoration: none; color : blue }
        -->
        </style>
        <html>
            <body>
                <a href=\"index_functions.html\">Function/Sub/Property Index</a><p />
                <a href=\"index_classes.html\">Class Index</a><p />
                <a href=\"index_variables.html\">Global Variable Index</a><p />

                <hr />
                <center><h2>File Index:</h2></center>
                <table>
                %s
                </table>
            </body>
        </html>
"
        (let in_order = 
             List.sort (fun file1 file2 -> compare_case_fold file1.file_name file2.file_name) filedocs
         in
         String.concat "\n" 
             (List.map
                  (fun filedoc -> sprintf "<tr><td><a href='%s'>%s &nbsp; &nbsp; </td><td> %s</td></tr>"
                       (filedoc.file_name ^ ".html")
                       filedoc.file_name
                       (try String.sub filedoc.file_comment 0 100 
                        with Invalid_argument _ -> filedoc.file_comment))
                  in_order));
    close_out chan

let prop_func_list (access, default, prop) =
    List.flatten [
        (match prop.prop_get with
             | None -> []
            | Some f -> [{f with func_name = Symbol.concat [prop.prop_name; Symbol.of_string " Get"]}]);
        (match prop.prop_let with
            | None -> []
            | Some f -> [{f with func_name = Symbol.concat [prop.prop_name; Symbol.of_string " Let"]}]);
        (match prop.prop_set with
            | None -> []
            | Some f -> [{f with func_name = Symbol.concat [prop.prop_name; Symbol.of_string " Set"]}]);
    ]


let output_function_index filename filedocs =
    let chan = open_out filename in
    fprintf chan "
        <style>
        <!--
        a { text-decoration: none; color : blue }
        -->
        </style>
        <html>
            <body>
                <center><h2>Function Index:</h2></center>
                <table>
                %s
                </table>
            </body>
        </html>
"
        (let in_order_funcs = 
             List.sort 
                 (fun (file1, class1, func1) (file2, class2, func2) ->
                      let comp = Symbol.compare func1.func_name func2.func_name in
                      if comp = 0 then
                          let compfiles = compare_case_fold file1 file2 in
                          if compfiles = 0 then Symbol.compare class1 class2 else compfiles
                      else
                                comp)
                 (Doc.get_funcs_with_origin filedocs)
         in
         String.concat "\n" 
             (List.map
                  (fun (filename, classname, func) -> 
                       sprintf "<tr><td><a href='%s'>%s %s &nbsp; &nbsp; </td><td>%s</td><td> %s</td></tr>"
                       (filename ^ ".html")
                       (Symbol.to_string func.func_name)
                       
                       ( if (Symbol.to_string classname) = "" 
                         then "" 
                         else "(" ^ (Symbol.to_string classname) ^ ")" )
                       
                       filename

                       (try String.sub func.func_comment 0 100 
                        with Invalid_argument _ -> func.func_comment))
                  in_order_funcs));
    close_out chan


let output_class_index filename filedocs = ()
let output_variable_index filename filedocs = ()

let rec htmldocs ?(outputdir = "htmldocs") ?(docs_so_far = []) files =
    match files with
        | [] ->
              List.iter (fun filedoc ->
                             let outputfile = (Filename.concat outputdir filedoc.file_name) ^ ".html" in
                             open_and_output_file_doc ~outputfile filedoc)
              docs_so_far;
              
              (* Output the indexes *)
              output_main_index (Filename.concat outputdir "index.html") docs_so_far;
              output_function_index (Filename.concat outputdir "index_functions.html") docs_so_far;
              output_class_index (Filename.concat outputdir "index_classes.html") docs_so_far;
              output_variable_index (Filename.concat outputdir "index_variables.html") docs_so_far;


        | file :: rest ->
              try
                  Printf.eprintf "Generating docs for %s...\n%!" file;
                  htmldocs ~outputdir ~docs_so_far:((doc_file file) :: docs_so_far) rest
              with
                  | Parsing.Parse_error -> 
                        eprintf "Parse error in %s, skipping.\n" file;
                        htmldocs ~outputdir ~docs_so_far rest
                      
