
(** A module for handling VbScript types, values, and scoping.  Anything
that is not mutually dependent upon {!Run.exec}, {!Run.eval} or {!Run.apply}
should go in here *)

open VbTypes

(** {4 Variant Helper Functions} *)

(** The conversions of the value 'empty' to an int/float/string are
probably only done for comparison operators in the original language,
but we do it all the time i guess *)

(** Gets an informative string about the value, for debugging output *)
val debug_string : value_t -> string

val get_bool : value_t -> bool
val get_int32 : value_t -> int32
val get_int : value_t -> int
val get_float : value_t -> float
val get_string : value_t -> string
val get_array : value_t -> array_t
val get_date : value_t -> date_t
val get_object : value_t -> object_t

val wrap_object : object_t -> value_t ref
val wrap_bool : bool -> value_t ref
val wrap_int32 : int32 -> value_t ref
val wrap_int : int -> value_t ref
val wrap_string : string -> value_t ref
val wrap_array : value_t ref array -> value_t ref
val wrap_float : float -> value_t ref
val wrap_date : date_t -> value_t ref
val wrap_byte : int -> value_t ref

val comfloat_of_netdate : Netdate.t -> float
val netdate_of_comfloat : float -> Netdate.t
val current_unixfloat : unit -> float
val netdate_of_unixfloat : float -> Netdate.t

(** {4 Array handling} *)
val create_array : int list -> array_t
val array_index : array_t -> int list -> value_t ref
val merge_arrays : array_t -> array_t -> array_t
val array_length : array_t -> int -> int

(** {4 Function Application} *)

(** Quick filters to return tuples of arguments, so that your match cases
	need not cause warnings, and do not have to constantly throw your own
	exceptions.  You pass the function name so that a pretty message can
	occur. *)

val arg0 : string -> value_t ref list -> unit
val arg1 : string -> value_t ref list -> value_t ref
val arg2 : string -> value_t ref list -> value_t ref * value_t ref
val arg3 : string -> value_t ref list -> value_t ref * value_t ref * value_t ref
val arg4 : string -> value_t ref list -> 
	value_t ref * value_t ref * value_t ref * value_t ref

(** Numeric handlers, to return ints when possible, floats when necessary, and
	to raise an exception when a value_t cannot be converted to either *)
val num_apply : 
	(int32 -> int32) -> 
	(float -> float) -> 
	value_t ref -> value_t

val num_apply2 :
	(int32 -> int32 -> int32) ->
	(float -> float -> float) ->
	value_t ref -> value_t ref -> value_t

(** Helper to apply a boolean function directly to bools, and bitwise to ints *)
val bool_apply : 
	(bool -> bool) ->
	(int32 -> int32) ->
	value_t ref -> value_t

val bool_apply2 :
	(bool -> bool -> bool) ->
	(int32 -> int32 -> int32) ->
	?true_null:value_t -> ?false_null:value_t ->
	value_t ref -> value_t ref -> value_t

(** [vb_compare] [val1] [val2] returns 0 if [val1] and [val2] are equal in a
	VbScript sense, and assorted other nearly-insane values depending on their
	type relations and values... check msdn.microsoft.com/scripting *)
val vb_compare : value_t ref -> value_t ref -> int

val create_function : 
    Symbol.t -> 
    ([`ByVal | `ByRef] * Symbol.t) list -> 
    'body ->
    'body user_defined_function_t


(** Returns a file that actually exists, if it does, in a case-insensitive
  way, or raises Not_found 

  NOTE: Apache's mod_spelling handles this for requests, its only when opening straight
  off the filesystem that we have to do it
*)
val get_file_casefold : string -> string
