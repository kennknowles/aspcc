

(** XML representations of ASP/VbScript Abstract Syntax Trees *)

(** TODO: add many entry points... consider caching as XML... or maybe even
	using Marshal to cache *)

(** {4 Functions} *)

(** [htmldocs ?outputdir filenames] Outputs documentation for all the
  files, and an index of the functions in all the files, to the given directory.

  Included are:
  {- for each file XXX/YYY/ZZZ.asp a file outputdir/XXX_YYY_ZZZ.asp.html}
  {- an index "index.html"}
  {- a function index "index_functions.html"}
  {- a class index "index_classes.html"}
  {- a global variable index "index_variables.html"}
*)

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
    file_todos : string list;
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

val doc_file : string -> file_doc
val doc_class : comment_buffer -> AspAst.class_def -> class_doc
val doc_function : comment_buffer -> AspAst.function_def -> function_doc
val doc_variable : AspAst.identifier -> var_doc

val get_funcs_with_origin : 
    ?so_far:(string * Symbol.t * function_doc) list -> 
    file_doc list -> 
    (string * Symbol.t * function_doc) list
