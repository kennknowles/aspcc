(** ASP/VbScript runtimes and customization/module loader *)

open VbTypes
open Tables

(** {4 Exceptions} *)

(** This exception is important because it is what any / all library functions
  should throw, methinks... *)
exception Error of string
exception ErrorAt of Lexing.position * AspAst.statement_body * string

(* TODO: replace Symbol_not_found with more specific exceptions? *)
exception Variable_not_found of Symbol.t
exception Function_not_found of Symbol.t
exception Class_not_found of Symbol.t
exception Member_not_found of Symbol.t * Symbol.t
exception Field_not_found of Symbol.t * Symbol.t
exception No_this


module Scope : sig
    type t = {
	    variables: (value_t ref) SymbolTable.t;
	    functions: function_t SymbolTable.t;
	    classes: class_t SymbolTable.t;
	    this : object_t option;
	    return : Symbol.t option
    }

    val create : unit -> t
    val copy : t -> t
end

    
(** {4 Types} *)

(** The type of runtime options for ASP/VbScript runtimes *)
type runtime_option = 
        [
        | `on_error_resume_next
        | `option_explicit
        | `use_cgi
        ]

(** The type of ASP/VbScript runtimes *)
type ('statement, 'expression) t = {
    mutable exec : ('statement, 'expression) exec_func;
    mutable eval : ('statement, 'expression) eval_func;
    mutable apply : ('statement, 'expression) apply_func;
	mutable output : string -> unit;
    mutable err : ('statement, 'expression) err_func;
	mutable script_path : string;
	scope : Scope.t;
	options : (runtime_option, bool) Hashtbl.t
}

and ('statement, 'expression) exec_func = 
        ('statement, 'expression) t -> 'statement -> unit

and ('statement, 'expression) eval_func = 
        ('statement, 'expression) t -> ?return_object:bool -> 'expression -> value_t ref

and ('statement, 'expression) apply_func = 
        ('statement, 'expression) t -> ('statement list) user_defined_function_t -> function_t

and ('statement, 'expression) err_func = ('statement, 'expression) t -> 
    ?pos:Lexing.position -> 
    ?statement:AspAst.statement_body -> 
    string -> 
    unit


(** {6 Creation/Modification of runtimes} *)

(** Creates a new runtime, with an empty global scope *)
(* TODO: investigate use of making output polymorphic *)
val create : 
    exec:('a, 'b) exec_func ->
    eval:('a, 'b) eval_func ->
    apply : ('a, 'b) apply_func ->
    output:(string -> unit) ->
    err : ('a, 'b) err_func ->
    ('a, 'b) t

(** [copy r] returns a fully independent copy of [r] *)
val copy : ('a, 'b)  t -> ('a, 'b) t
    
val set_option : ('a, 'b) t -> runtime_option -> bool -> unit
val get_option : ('a, 'b) t -> runtime_option -> bool
    
val add_function : ('a, 'b) t -> Symbol.t -> function_t -> unit
val add_variable : ('a, 'b) t -> Symbol.t -> value_t ref -> unit
val add_class : ('a, 'b) t -> Symbol.t -> class_t -> unit
val add_object : ('a, 'b) t -> of_class:Symbol.t -> Symbol.t -> unit

val get_variable : ('a, 'b) t -> Symbol.t -> value_t ref
val get_class : ('a, 'b) t -> Symbol.t -> class_t
val get_function : ('a, 'b) t -> Symbol.t -> function_t
val get_this : ('a, 'b) t -> object_t

val variable_exists : ('a, 'b) t -> Symbol.t -> bool
val function_exists : ('a, 'b) t -> Symbol.t -> bool
val class_exists : ('a, 'b) t -> Symbol.t -> bool
    
val raise_error : string -> 'a
val raise_error_at : Lexing.position -> AspAst.statement_body -> string -> 'a
val string_of_error : Lexing.position -> AspAst.statement_body -> string -> string


val dump : ('a, 'b) t -> string

(** {6 Module registration / loading functions} *)

(* TODO: get some polymorphism into module loading *)
type restricted_t = (AspAst.statement, AspAst.rvalue) t

type load_function = restricted_t -> unit
    
(** [register_module name loader] will add the module [name] to the
  end of the list, only if it is not already present *)
val register_module : string -> load_function -> unit
    
(** Apply a named module to your runtime *)
val apply_module : restricted_t -> string -> unit
    
(** Apply all modules in the order that they registered themselves *)
val apply_all_modules : restricted_t -> unit

val module_names : unit -> string list






(* 
   Features to possibly add; a +/- indicates the default value:
   -	omitthen	: allow omitting the 'then' from if statements, when \n present
   -	dimassign	: assign a var on the same line as dimming it
   -	localclass	: declare a class only in local scope
   -	localfuncs	: declare a function only in local scope
   -	localdim	: declare a variable local to blocks, not just subs
   -	casesensitive
   -	print_r		: automatic printing of array and object valuese
   +	intrinsics	: include the ASP intrinsic objects and classes
   +	adovbs		: include the ADOVBS constants
   +	robustdefinition : don't error when functions/classes are redefined
   +	dictionary	: include the Scripting.Dictionary class
   -	xml			: include Microsoft.XML related classes, requires PXP
   -	mail		: include the CDonts.Mail class, requires ocamlnet
   +	robustdates	: include better date parsing, requires ocamlnet
   -	escapechars	: interpret things like \n and \t in strings
   -	robustis	: allow the 'is' operator to work on non-objects
   -	optionalargs	: allow default values for arguments/optional arguments
   -	returnkeyword	: allow use of 'return' instead of dumb vbscript convention
   -	robustarrays	: allow incomplete indices on arrays to return arrays

   -	httpenv		: grab GET POST and SESSION variables from the environment,
   to make commandline tests easier
   -	warnings	: setting to on will output some warnings

   +	error		: setting to off is the same an 'on error resume next'
   -	explicit	: setting on is the same as 'option explicit'


   type on_error_t = GotoZero | ResumeNext

   type runtime_options_t = {

(* ASP helper options *)  (* mark those that should be modules with a (**) *)
   mutable adovbs : bool;		(**)
   mutable dictionary : bool;	(**)
   mutable xml : bool;			(**)
   mutable intrinsics : bool;	(**)
   mutable mail : bool;		(**)
   mutable httpenv : bool;		(**)

(* vbscript bastardization options *)
   mutable escapechars : bool;
   mutable localfuncs : bool;
   mutable localclass : bool;
   mutable localdim : bool;
   mutable print_r : bool;		(**)
   mutable dimassign : bool;
   mutable omitthen : bool;
   
(* compatability or limitation options *)
   mutable robustdates : bool;

   mutable warnings : bool;
(* options toggle-able from the code *)
   mutable explicit : bool;
   mutable on_error : on_error_t
   }

   let runtime_options = {
(* ASP helper options *)
   adovbs = true;
   dictionary = true;
   xml = false;
   intrinsics = true;
   mail = false;
   httpenv = false;

(* vbscript bastardization options *)
   escapechars = false;
   localfuncs = false;
   localclass = false;
   localdim = false;
   print_r = false;
   dimassign = false;
   omitthen = false;

(* turn this on to be truly compatible, but adds a dependency *)	
   robustdates = false;

   warnings = false;
(* options toggle-able from the code *)
   explicit = false;
   on_error = GotoZero;
   }
*)
