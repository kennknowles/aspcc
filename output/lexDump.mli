

(** Dumping a poorly formatted stream of ASP/VbScript tokens, labelled *)

(** Really only useful for figuring out why it won't parse.  More effective is 
	setting OCAMLRUNPARAM=p in your environment *)

(** {4 Functions} *)

(** Print a single token to stdout *)
val print_token : AspParser.token -> unit
