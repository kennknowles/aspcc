(** Dumping of Vb syntax, for pretty error messages mostly *)


(** {4 Functions} *)

val string_of_statement : AspAst.statement_body -> string
val string_of_rvalue : AspAst.rvalue -> string
val string_of_id : AspAst.identifier -> string
