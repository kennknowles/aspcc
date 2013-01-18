
(** A module containing the syntax for ASP/VbScript pages, including
	all inlining and server-side includes (we have to emulate IIS's way of doing them) *)

(** {4 Expressions} *)

type binary_operator =
	| Concat
	| Add | Subtract | Mult | Div | IntDiv | Mod | Exp
	| And | Or | Xor | Imp | Eqv
	| Equals | NotEquals | Less | Greater | LessEqual | GreaterEqual
	| Is

type unary_operator =
	| Negative | Not
	
type integer_text_format = Hex | Oct | Dec | Bin



(** The types of identifiers *)
type identifier = 
	| AtomicId of Symbol.t			(** A single, simple, name*)
	| Dot of identifier * Symbol.t	(** Dereferencing a field of an object *)
	| Indices of identifier * rvalue list	(** Indexing into an object; function apps also look like this *)

(** The types of rvalues, general expressions.  TODO: make all binary operators one extensible construct *)
and rvalue =
	| Int of integer_text_format * int32
    | Float of float
    | String of string
	| Bool of bool

	| BinaryOp of binary_operator * rvalue * rvalue
	| UnaryOp of unary_operator * rvalue

    | Identifier of identifier
	| Eval of rvalue	(** TODO: make this a standard lib function, instead of syntactic construct. *)

	| New of identifier

	| Null
	| Nothing

and constant_definition =
	Symbol.t * rvalue

and exit_target = [`Do | `While | `For | `Function | `Sub | `Property | `With]

(* To avoid introducing a keyword, the Callback will reuse the token GOTO *)
type error_target = GotoZero | ResumeNext | Callback of identifier

type statement = Lexing.position * statement_body
and statement_body =
    | EmptyStatement
	| Call of identifier
	| Class of class_def
		(** not a CompoundStatement because it isn't control flow *)
	| Comment of string
	| Const of constant_definition
    | Dim of identifier list
		(** not correct, but easy enough for now *)
	| Erase of identifier
	| Execute of rvalue	(** TODO: make this a standard lib function, instead of syntactic construct. *)
	| Exit of exit_target
	| Function of function_def
	| If of rvalue * (statement list) * (statement list) (** predicate, then_branch, else_branch *)
	| MemberDef of access_level * bool * memberstatement (** access, is_default, definition *)
	| OnError of error_target
	| OptionExplicit
	| Randomize		(** TODO: make this a standard lib function instead of syntactic construct *)
    | ReDim of bool * (identifier list)
		(** whether_to_preserve, identifiers *)
	| Select of rvalue * (selectstatement list)
	| Set of identifier * rvalue
	| Ssi of string * string
	| Sub of function_def
	| SubCall of identifier * (rvalue list)

	| CompoundStatement of compound_statement * (statement list)

    | Assignment of identifier * rvalue
	| Html of string 
	| InlineExpr of rvalue

(** All members that aren't mere variables are unified into functions in the backend -
	vb's idea of subs vs functions vs properties is dumb as hell *)
and memberstatement =
	| MemberIdent of identifier list
	| MemberFunction of function_def
	| MemberSub of function_def
	| PropertyGet of function_def
	| PropertyLet of function_def
	| PropertySet of function_def

and selectstatement = selectitem * (statement list)
and selectitem = SelectValue of rvalue list | SelectElse

and access_level = [`Private | `Public]

and function_def = Symbol.t * ((byval * Symbol.t) list) * (statement list)
(*and sub = string * (string list) * (statement list)*)
and class_def = Symbol.t * (statement list)

and compound_statement =
	| For of Symbol.t * rvalue * rvalue * rvalue  (** ident, start, end, increment *)
	| ForEach of Symbol.t * rvalue		(** single_item, container_to_iterate *)
	| While of rvalue
	| DoWhile of rvalue
	(* Until of rvalue*)
	| With of identifier		(** object identifier, not rvalue?? *)

and byval = [`ByVal | `ByRef]

type page = statement list
        
