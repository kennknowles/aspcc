(** ASP/VbScript runtimes and customization/module loader *)

open VbTypes

type runtime = (AspAst.statement, AspAst.rvalue) Runtime.t

val create_runtime : unit -> runtime

(** {6 Execution entry points} *)

(** Runs a whole page, or list of statements *)
val page : runtime -> AspAst.page -> unit

(** Runs a single statement *)
val statement : runtime -> AspAst.statement -> unit

(** Evaluates a single expression.  If [return_object] is [true], then it
	will return a raw object, otherwise it will evaluate default properties
	until a non-object is obtained. *)
val expression : runtime -> ?return_object:bool -> AspAst.rvalue -> value_t ref
