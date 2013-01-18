
(** A module for user-defined vb classes; currently (probably forever) depends on AspAst *)
open VbTypes
open Tables

(** The class of user-defined, transparent classes (as opposed to builtin,
	opaque classes *)
class user_defined_class :
    Runtime.restricted_t ->
    Symbol.t -> 
object
	method name : Symbol.t
    method fields : (AspAst.access_level * int list option) SymbolTable.t
        
	method get_default : Symbol.t option
	method set_default : Symbol.t -> unit
        
	(** Add a field to the class (useful for traversing to AST) *)
	method add_field : Symbol.t -> ?privacy:AspAst.access_level -> int list option -> unit
	
	(** Add a property/function to the class (useful for traversing to AST) *)
	method add_property : 
        ?action:property_action ->
        ?privacy:AspAst.access_level -> 
        Symbol.t -> 
        (AspAst.byval * Symbol.t) list ->
        AspAst.statement list ->
        unit

    method get_property :
        ?action:property_action ->
        ?privacy:AspAst.access_level ->
        Symbol.t ->
        object_t ->
        function_t

    method create_object : object_t
end

(** Helper class of objects that just raises an exception for any of its
	methods.  Your opaque class can extend this one, and override only that
	which you want.
  
  Sinc a human and not a parser is interacting with it, there are string-based
  wrappers are symbol-type functions.  You should override the m_gets not the m_get
 *)
class virtual opaque_object :
object
    method classname : Symbol.t
        
    method virtual strname : string

    (* This will actually never return, it always raises an exception *)
    method field : Symbol.t -> value_t ref
    method m_get : Symbol.t -> value_t ref list -> value_t ref
    method m_let : Symbol.t -> value_t ref list -> value_t ref
    method m_set : Symbol.t -> value_t ref list -> value_t ref
    
    method invalid_arg_count : string -> int -> value_t ref list -> value_t ref
    method not_found : string -> value_t ref list -> value_t ref
    method m_gets : string -> value_t ref list -> value_t ref
    method m_lets : string -> value_t ref list -> value_t ref
    method m_sets : string -> value_t ref list -> value_t ref
end

(** This class is intended to be instantiated inline, rather than inherited
  from, so it doesn't have virtual, but rather takes it as a parameter *)
class collection : 
    string ->
    value_t ref array ->
object
    method classname : Symbol.t

    method strname : string

    method field : Symbol.t -> value_t ref

    method m_get : Symbol.t -> value_t ref list -> value_t ref
    method m_let : Symbol.t -> value_t ref list -> value_t ref
    method m_set : Symbol.t -> value_t ref list -> value_t ref
    
    method invalid_arg_count : string -> int -> value_t ref list -> value_t ref
    method not_found : string -> value_t ref list -> value_t ref
    method m_gets : string -> value_t ref list -> value_t ref
    method m_lets : string -> value_t ref list -> value_t ref
    method m_sets : string -> value_t ref list -> value_t ref
end
        
(* TODO
class dictionary : 
    string ->
    value_t ref array ->
object
    method classname : Symbol.t

    method not_found : Symbol.t -> value_t ref list -> value_t ref
    method m_get : Symbol.t -> value_t ref list -> value_t ref
    method m_let : Symbol.t -> value_t ref list -> value_t ref
   method m_set : Symbol.t -> value_t ref list -> value_t ref
end
*) 

