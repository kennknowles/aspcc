
(** A module for handling VbScript types, values, and scoping.  Anything
that is not mutually dependent upon {!Run.exec}, {!Run.eval} or {!Run.apply}
should go in here *)

(** {2 Types} *)

(** Types of actions a property can have *)
type property_action = [`Get | `Set | `Let]

(***********************************************************)
(** {6 Parameterized types to workaround mutual recursion} *)
(***********************************************************)

(** A parameterized class of objects that can hold any value, and return
	any type of dispatch.  This is needed to work around the mutually
	recursive definitions of values and objects *)
class type ['value] parameterized_object_t = object
    method classname : Symbol.t
    method field : Symbol.t -> 'value ref
    method m_get : Symbol.t -> 'value ref list -> 'value ref
    method m_let : Symbol.t -> 'value ref list -> 'value ref
    method m_set : Symbol.t -> 'value ref list -> 'value ref
end

type 'a parameterized_class_t = unit -> 'a

(** See VbTools.parameterized_object_t for why we need this *)
type 'a recursive_array_t =
	| Single of 'a ref array
	| Multi of 'a recursive_array_t array


(***********************************************************)
(** {6 Types of values} *)
(***********************************************************)

(** The type of VbScript values *)
and value_t = 
	| Null
	| Empty
	| Int of int32
	| Float of float 
	| String of string 
	| Bool of bool
	| DateTime of date_t   
(*    | DateTime of float -- This is how COM and VbScript store dates, but I don't have a library
      that does spiffy calculations on them, so instead I'll have functions to pull a date to/from
      float *)
	| Array of array_t
	| Object of (object_t ref) option (* can be 'nothing' i.e. None *)

(** The type of DateTime and components **)
and datetype_t =  Date | Time | Both 
and date_t = datetype_t * Netdate.t

(** The type of VbScript arrays *)
and array_t = value_t recursive_array_t

(** The type of vb objects *)
and object_t = value_t parameterized_object_t

(** The type of vb classes *)
and class_t = object_t parameterized_class_t

(** The type of VbScript functions *)
and function_t = value_t ref list -> value_t ref

(** The type of user-defined VbScript functions *)
and 'body user_defined_function_t = {
	f_name : Symbol.t;		(** The function's name *)
	f_args : (byval_t * Symbol.t) list;
	(** A list of arguments, paired with a boolean to indicate if it is
	  mandatory - note that MS does not support optional arguments, so
	  we don't either... yet!  Also, we do not yet support ByVal or
		ByRef... I think everything is ByRef right now *)
	f_body : 'body
		(** The actual statements inside the function *)
}

and byval_t = [`ByVal | `ByRef]

(***********************************************************)
(** {2 Exceptions} *)
(***********************************************************)

(** [No_such_member (classname, membername)] *)
exception No_such_member of string * string 

(** Raised whenever a variant is used as an incompatible type *)
exception Cannot_convert of value_t * string

(** Provided so that modules can define placeholder functions that can just
	raise this until they are completed *)
exception Not_implemented of string

(** Raised when a non-object is dereferenced or set *)
exception Not_object of value_t

(** Raised when a non-array is indexed into, or redimmed *)
exception Not_array of value_t

exception Array_out_of_bounds

(** Raised when the number of indices supplied doesn't match the dimensions
	of the array - eventually we may support partial indexing, but MS doesn't so
	it isn't a priority *)
exception Invalid_indices of value_t * int list

(** Raised when an argument list to a function is not the right length *)
(* TODO: include the given and expected lengths *)
exception Invalid_arg_list of string

(** The new exception that should be raised is 
	[Invalid_arg_count func expected_num given_num] *)
exception Invalid_arg_count of string * int * int

exception Class_field_not_found of string * string (** [classname, fieldname] *)
