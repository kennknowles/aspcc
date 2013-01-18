
(** [htmldocs ?outputdir filenames] Outputs documentation for all the
  files, and an index of the functions in all the files, to the given directory.

  Included are:
  {- for each file XXX/YYY/ZZZ.asp a file outputdir/XXX_YYY_ZZZ.asp.html}
  {- an index "index.html"}
  {- a function index "index_functions.html"}
  {- a class index "index_classes.html"}
  {- a global variable index "index_variables.html"}
*)

val htmldocs : ?outputdir:string -> ?docs_so_far:Doc.file_doc list -> string list -> unit
