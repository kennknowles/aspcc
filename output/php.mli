
(** Output of equivalent PHP code *)

(** Outputs equivalent, and intuitive, PHP code.  Some vbscript functions are
	implemented in the PHP file $(ASPCC_SRCDIR)/php/asp_implementations.php,
	but whenever possible an equivalent PHP builtin is output. *)

(** TODO: Add more entry points, parameterize the output file/channel,
	implement all of the vb standard lib, and somehowe plug in to the runtime
	so that we can determine more of the types of things *)

(** {4 Functions} *)

(** Print the equivalent of the page to stdout *)
val print_page : AspAst.page -> unit
