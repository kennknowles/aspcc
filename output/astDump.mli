
(** XML representations of ASP/VbScript Abstract Syntax Trees *)

(** TODO: add many entry points... consider caching as XML... or maybe even
	using Marshal to cache *)

(** {4 Functions} *)

(** Prints an entire page, parsed, as XML *)
val print_page : AspAst.page -> unit
