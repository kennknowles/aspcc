
type var_doc = {
    var_name : Symbol.t;
    var_comment : string
}

type function_doc = {
    func_name : Symbol.t;
    func_comment : string;
    func_todos : string list;
    func_depends : string list;
    func_params : var_doc list;
    func_vars : var_doc list;
    func_return : string
}

type property_doc = {
    prop_name : Symbol.t;
    prop_get : function_doc option;
    prop_let : function_doc option;
    prop_set : function_doc option
}
    

type class_doc = {
    class_name : Symbol.t;
    class_comment : string;
    class_todos : string list;
    class_vars : (AspAst.access_level * var_doc) list;
    class_depends : string list;
    class_functions : (AspAst.access_level * bool * function_doc) list;
    class_subs : (AspAst.access_level * bool * function_doc) list;
    class_props : (AspAst.access_level * bool * property_doc) list
}                     

type file_doc = {
    file_name : string;
    file_comment : string;
    file_todos: string list;
    file_depends : string list;
    file_includes : string list;
    file_classes : class_doc list;
    file_functions : function_doc list;
    file_subs : function_doc list;
    file_vars : var_doc list
}

type comment_buffer = {
    buf_recording : bool;
    buf_comment : string;
    buf_params : var_doc list;
    buf_return : string;
    buf_depends : string list;
    buf_todos: string list;
}

let empty_buffer = {
    buf_recording = false;
    buf_comment = "";
    buf_params = [];
    buf_return = "";
    buf_depends = [];
    buf_todos = []
}


open AspAst
open Printf

let ws_regex = Str.regexp "[ \t\r\n]+"

let rec ltrim ?(pos = 0) s =
    if pos >= String.length s then
        ""
    else if s.[pos] = ' ' or s.[pos] = '\t' then
        ltrim ~pos:(pos+1) s
    else
        String.sub s pos ((String.length s) - pos)

exception Start_recording

(* This function doesn't know if we are recording, so it only handles things
   that are significant whether or not we are *)
let process_comment buffer text =
    let trimmed = ltrim text in
    let split = Str.split ws_regex trimmed in
    match split with
        | "@todo" :: rest ->
              {buffer with buf_todos = buffer.buf_todos @ [trimmed]}

        | "@param" :: name :: rest ->
              {buffer with buf_params = buffer.buf_params @ 
                                        [{var_name = (Symbol.of_string name);
                                          var_comment = String.concat " " rest}]}

        | "@returns" :: rest ->
              {buffer with buf_return = String.concat " " rest}
              
        | "@depends" :: rest ->
              {buffer with buf_depends = buffer.buf_depends @ [String.concat " " rest]}
              
        | "@@" :: _ ->
              (*printf "Recording is now %B\n" (not buffer.buf_recording);*)
              {buffer with buf_recording = not buffer.buf_recording}
              
        | _ -> 
              if buffer.buf_recording then
                  ((*printf "Adding '%s' to comment.\n" text;*)
                  {buffer with buf_comment = buffer.buf_comment ^ "\n" ^ text})
              else
                  buffer

let xor a b = (a || b) && (not (a && b))

(* Clearing functions *)
let clear_func_entries buf =
    {buf with
         buf_recording = false;
         buf_comment = "";
         buf_params = [];
         buf_return = ""}

let clear_class_entries buf =
    {buf with
         buf_recording = false;
         buf_comment = ""}

let clear_dim_entries buf = buf

(* Simple var doc'ing *)
let rec doc_variable id =
    match id with
        | AtomicId name -> {var_name = name; var_comment = ""}
        | Dot (id, member) ->
              let rootdoc = doc_variable id in
              {rootdoc with
                   var_name = Symbol.of_string 
                                  ( (Symbol.to_string rootdoc.var_name) ^ "." ^ 
                                    (Symbol.to_string member) ) }
              
        | Indices (id, rvals) -> 
              let rootdoc = doc_variable id in
              {rootdoc with 
                   var_name = Symbol.of_string (sprintf "%s(%s)"
                                                    (Symbol.to_string rootdoc.var_name)
                                                    (String.make (List.length rvals) ','))}
                                  

(* Function doc'ing *)
let fold_func_statement (buffer, funcdoc) (_, statement) =
    match statement with
        | Comment c -> process_comment buffer c, funcdoc
        | Dim ids -> clear_dim_entries buffer, {funcdoc with
                                                    func_vars = funcdoc.func_vars @ 
                                                                (List.map doc_variable ids)}
        | _ -> buffer, funcdoc

let rec find_param param paramdocs =
    match paramdocs with
        | [] -> false
        | var :: rest ->
              if var.var_name = param then
                  true
              else
                  find_param param rest

(* TODO: fix the ordering of the params - use an array probably *)
let doc_function buffer (name, params, body) =
    let paramdocs =
        List.fold_left 
            (fun docs (byval, param) ->
                 if find_param param docs then
                     docs
                 else
                     {var_name = param; var_comment = ""} :: docs)
            buffer.buf_params
            params
    in
    snd (List.fold_left
             fold_func_statement
             ( empty_buffer,
               {
                   func_name = name;
                   func_comment = buffer.buf_comment;
                   func_todos = [];
                   func_depends = [];
                   func_params = paramdocs;
                   func_vars = [];
                   func_return = buffer.buf_return
               } )
             body)


(* Class doc'ing *)
type prop_type = Get | Set | Let

let empty_prop = {
    prop_name = Symbol.empty;
    prop_get = None;
    prop_let = None;
    prop_set = None
}

(* I don't want to ruin the symmetry of my data structs by using a Map, this is a less
   efficient home-brewed thing *)
let rec add_prop ?(keeplist = []) proplist (access, default, addedprop) proptype =
    match proplist with
        | [] -> 
              let prop = {empty_prop with prop_name = addedprop.func_name} in
              keeplist @
              [(access, default,
                match proptype with
                    | Get -> {prop with prop_get = Some addedprop}
                    | Let -> {prop with prop_let = Some addedprop}
                    | Set -> {prop with prop_set = Some addedprop})]
                   
        | (acc, def, prop) :: rest ->
              if prop.prop_name = addedprop.func_name then
                  keeplist @
                  ((acc, def, 
                    match proptype with
                        | Get -> {prop with prop_get = Some addedprop}
                        | Let -> {prop with prop_let = Some addedprop}
                        | Set -> {prop with prop_set = Some addedprop}) :: rest)
              else
                  add_prop ~keeplist:(keeplist @ [access, default, prop]) 
                      rest (access, default, addedprop) proptype
                      
        
let fold_class_statement (buffer, classdoc) (_, statement) =
    match statement with
        | Comment c -> process_comment buffer c, classdoc

        | Dim ids -> buffer, {classdoc with
                                  class_vars = List.map 
                                                   (fun id -> `Public, doc_variable id) 
                                                   ids }

        | MemberDef (access, default, member) -> (
              match member with
                  | MemberIdent ids ->
                        clear_dim_entries buffer,
                        {classdoc with
                             class_vars = classdoc.class_vars @ 
                                          (List.map
                                               (fun id -> access, doc_variable id)
                                               ids) }
                            
                  | MemberFunction f ->
                        clear_func_entries buffer,
                        {classdoc with
                             class_functions = classdoc.class_functions 
                                               @ [access, default, doc_function buffer f]}
                            
                  | MemberSub s ->
                        clear_func_entries buffer,
                        {classdoc with
                             class_subs = classdoc.class_functions 
                                          @ [access, default, doc_function buffer s]}

                  | PropertyGet f
                  | PropertyLet f
                  | PropertySet f ->
                        let proptype = match member with
                            | PropertyGet _ -> Get
                            | PropertyLet _ -> Let
                            | PropertySet _ -> Set
                        in
                        clear_func_entries buffer,
                        {classdoc with 
                             class_props = add_prop 
                                               classdoc.class_props
                                               (access, default, doc_function buffer f)
                                               proptype}
          )
        | _ -> buffer, classdoc
                        
    
                  
                    
let doc_class buffer (name, body) =
    snd (List.fold_left
             fold_class_statement
             ( empty_buffer,
               {
                   class_name = name;
                   class_comment = buffer.buf_comment;
                   class_todos = [];
                   class_vars = [];
                   class_depends = [];
                   class_functions = [];
                   class_subs = [];
                   class_props = []
               } )
               body)


(* Global file level doc'ing *)
let fold_file_statement (buffer, filedoc) (_, statement) =
    match statement with
        | (Comment c) ->
              let newbuf = process_comment buffer c in
              (* if we just toggled recording off, then it is a free-floating comment, to be
                 added to the file *)
              if buffer.buf_recording && (not newbuf.buf_recording) then
                  {newbuf with buf_comment = ""}, 
                  {filedoc with file_comment = filedoc.file_comment ^ newbuf.buf_comment}
              else
                  newbuf, filedoc
     
        | MemberDef (_,_, MemberFunction f)
        | Function f ->
              let func = doc_function buffer f in
              clear_func_entries buffer,
              {filedoc with
                   file_functions = filedoc.file_functions @ [func];
                   file_depends = filedoc.file_depends @ func.func_depends}
        
        | MemberDef (_,_, MemberSub f)
        | Sub f ->
              let func = doc_function buffer f in
              clear_func_entries buffer,
              {filedoc with
                   file_subs = filedoc.file_subs @ [func];
                   file_depends = filedoc.file_depends @ func.func_depends}

        | Class c ->
              let classdoc = doc_class buffer c in
              clear_class_entries buffer, 
              {filedoc with
                   file_classes = filedoc.file_classes @ [classdoc];
                   file_depends = filedoc.file_depends @ classdoc.class_depends}
              
        | MemberDef (_,_, MemberIdent ids)
        | Dim ids ->
              let iddocs = List.map doc_variable ids in
              clear_dim_entries buffer,
              {filedoc with
                   file_vars = filedoc.file_vars @ iddocs}

        | Ssi (kind, filename) ->
              buffer,
              {filedoc with
                   file_includes = filedoc.file_includes @ [filename]}

        | _ -> buffer, filedoc


                                     
open Lexing
let doc_file filename =
    let lexbuf = Lexing.from_channel (open_in filename) in
    lexbuf.lex_curr_p <- 
	{
		pos_fname = filename;
		pos_lnum = 1;
		pos_bol = 0;
		pos_cnum = 0;
	};
    let ast = AspParser.page (AspLexer.token AspLexer.Html) lexbuf in
    let buffer, filedoc =
        (List.fold_left fold_file_statement 
             ( empty_buffer,
               {
                   file_name = filename;
                   file_comment = "";
                   file_todos = [];
                   file_depends = [];
                   file_includes = [];
                   file_classes = [];
                   file_functions = [];
                   file_subs = [];
                   file_vars = []
               })
             ast)
    in
    {filedoc with
         file_depends = filedoc.file_depends @ buffer.buf_depends;
         file_todos = filedoc.file_todos @ buffer.buf_todos;
    }
    
let compare_case_fold s1 s2 =
    compare (String.uppercase s1) (String.uppercase s2)

let prop_func_list (access, default, prop) =
    List.flatten [
        (match prop.prop_get with
             | None -> []
             | Some f -> [{f with 
                               func_name = Symbol.concat [prop.prop_name; Symbol.of_string " Get"]}]);
        
        (match prop.prop_let with
             | None -> []
             | Some f -> [{f with 
                               func_name = Symbol.concat [prop.prop_name; Symbol.of_string " Let"]}]);
        
        (match prop.prop_set with
             | None -> []
             | Some f -> [{f with 
                               func_name = Symbol.concat [prop.prop_name; Symbol.of_string " Set"]}])
    ]

let third (one,two,three) = three

let rec get_class_members_with_origin ?(so_far = []) filename classdocs =
    match classdocs with
        | [] -> so_far 
        | classdoc :: rest ->
              let additions =
                  (List.map (fun func -> (filename, classdoc.class_name, func))
                       (List.flatten [
                            List.map third classdoc.class_functions;
                            List.map third classdoc.class_subs;
                            List.flatten (List.map prop_func_list classdoc.class_props)
                        ]) )
              in
              get_class_members_with_origin ~so_far:(so_far @ additions) filename rest
                         

let rec get_funcs_with_origin ?(so_far = []) filedocs =
    match filedocs with
        | [] -> so_far
        | filedoc :: rest ->
              let additions =
                  List.flatten [
                      (List.map (fun func -> (filedoc.file_name, Symbol.empty, func)) filedoc.file_functions);
                      (List.map (fun func -> (filedoc.file_name, Symbol.empty, func)) filedoc.file_subs);
                      get_class_members_with_origin filedoc.file_name filedoc.file_classes
                  ]
              in
              get_funcs_with_origin ~so_far:(so_far @ additions) rest

