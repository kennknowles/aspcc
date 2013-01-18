open Printf

(** A module containing the syntax for ASP/VbScript pages, including
	all inlining and server-side includes (we have to emulate IIS's way of doing them) *)

let warn s = eprintf "Warning: %s\n" s
let error s = eprintf "Error: %s\n" s

type scalar_type = [
| `Unknown
| `String
| `Int
| `Numeric
| `Boolean
| `DateTime
| `Float
| `Scalar
]

type numeric_type = [
| `Numeric
| `Int
| `Float
]

(* Some categories are fatal *)
type error_category = [
| `Imprecise_scalar
| `Imprecise_object
| `Incompatible_type
| `No_method
]

(** {4 Expressions} *)

type nil_type = [
| `Empty
| `Null
| `Nothing
]
   
module TypeMap = Map.Make(Symbol)

type vb_type = [
| scalar_type 
| nil_type
| `Unknown
| `Func of (vb_type list) * vb_type
| `Unknown_object
| `Object of environment
| `Class of environment
| `Error of vb_type * error_category * string
| `Array
]

and environment = vb_type TypeMap.t

type object_type = [
| `Unknown_object
| `Object of vb_type TypeMap.t
]

(* The list of changes variable types... for now only statements support this,
   not expressions *)
type binding_changes = (string * vb_type) list
        
    

(* Checks over the tree for warnings and errors *)
(*let traverse ?(env = TypeMap.empty) ast =
    match ast with
        | [] ->
        | (binding_changes, expressions) :: rest ->
              let new_env =
                  List.fold_left (fun environ (varname, newtype) -> 
                                      TypeMap.add varname newtype environ)
                      env
                      binding_changes
              in
              
              List.iter traverse_expression expressions

              traverse ~env:new_env rest
*)                                

(*type typed_ast = [
| `expression of newbinding
|
]*)

let string_of_type = function
    | `Unknown -> "unknown"
    | `String -> "string"
    | `Int -> "int"
    | `Boolean -> "bool"
    | `DateTime -> "datetime"
    | `Float -> "float"
    | `Scalar -> "scalar"
    | `Unknown_object -> "unknown_object"
    | `Object _ -> "object"
          
    | `Empty -> "empty"
    | `Null -> "null"
    | `Nothing -> "nothing"

    | `Numeric -> "numeric"
          
    | _ -> "<<>>"


open AspAst

let typemap_of_list li =
    List.fold_left (fun map (s,t) -> TypeMap.add (Symbol.of_string s) t map)
        TypeMap.empty
        li

let builtins =
    typemap_of_list [
        
        "response", `Object (typemap_of_list [
                                 "write", `Func ([`String], `Empty)
                             ])
            
    ]

let unify goal actual =
    match goal, actual with
        | `String, #scalar_type -> true
              
        | `Numeric, #numeric_type -> true
        | `Numeric, (#scalar_type as t) -> (
              warn (sprintf 
                        "Non-numeric scalar of type %s used where numeric expected"
                        (string_of_type t)); true)
           
        | `Boolean, (#numeric_type as t) -> (
              warn (sprintf
                        "Non-boolean scalar of type %s used where boolean expected."
                        (string_of_type t)); true)

        | `Boolean, #object_type -> warn (sprintf
                                              "Not yet checked: object used in boolean context."); true

        | _, _ -> goal = actual


let type_unary_op op =
    let param, return = 
        match op with
            | Negative -> (`Numeric, `Numeric)
            | Not -> (`Boolean, `Boolean)
    in
    `Func ([param], return)

let type_binary_op op =
    let param, return =
        match op with
	    | Concat -> `String, `String
	    | Add 
        | Subtract
        | Mult 
        | Div
        | IntDiv 
        | Mod
        | Exp -> `Numeric, `Numeric
	    | And 
        | Or
        | Xor
        | Imp 
        | Eqv -> `Boolean, `Boolean

	    | Equals 
        | NotEquals -> `Unknown, `Boolean

        | Less
        | Greater 
        | LessEqual
        | GreaterEqual -> `Numeric, `Boolean
	    | Is -> `Unknown_object, `Boolean
              
    in
    `Func ([param; param], return)

let rec type_identifier env id : vb_type =
    match id with
            
        (* := name : 'a *)
        | AtomicId name ->  (
              try
                  TypeMap.find name env
              with
                  | Not_found ->
                        error (sprintf "Variable %s is undefined." (Symbol.to_string name));
                        `Unknown
          )

        (* := id : < name : 'a >
           ---
           := id.name : 'a *)
        | Dot (id, name) -> 
              match type_identifier env id with
                  | `Unknown_object -> 
                        warn (sprintf 
                                  "Dereferencing field %s of unknown object." 
                                  (Symbol.to_string name));
                        `Unknown
                            
                  | `Object objmap -> (
                        try
                            TypeMap.find name objmap
                        with
                            | Not_found ->
                                  error (sprintf "Object has no such member: %s" (Symbol.to_string name));
                                  `Unknown
                    )
                        
                  | `Func _ as functype -> type_application env functype []

                  | _
                  | #nil_type
                  | #scalar_type as t ->
                        error (sprintf 
                                   "Dereferencing a non-object of type %s" 
                                   (string_of_type t));
                        `Unknown

and type_expression env expr : vb_type =
    match expr with
	    | Int _ -> `Int
        | Float _ -> `Float
        | String _ -> `String
	    | Bool _ -> `Boolean
        | Null -> `Null
        | Nothing -> `Nothing

        | Eval _ -> `Unknown (* TODO: fix *)
        | Identifier id -> type_identifier env id

        | BinaryOp (op, val1, val2) ->
              type_application env (type_binary_op op) [type_expression env val1;
                                                        type_expression env val2]
        
        | UnaryOp (op, val1) ->
              type_application env (type_unary_op op) [type_expression env val1]

        (* | New name -> (TypeMap.find name env) *)

and type_application env functype argtypes : vb_type =
    match functype, argtypes with

        (* := func : 'a->'b && arg : 'a
           ---
           := func arg : 'ab *)
        | `Func ([], return), [] -> return
              
        | `Func (firstarg :: restargs, return), (firstval :: restvals) ->
              if unify firstarg firstval then
                  type_application env (`Func (restargs, return)) restvals
              else (
                  error (sprintf "Value of type %s used where %s expected."
                             (string_of_type firstval)
                             (string_of_type firstarg));
                  `Unknown
              )
              
     
        | `Unknown, _ -> warn "Unknown function type applied."; `Unknown


        | _, _ -> error ("Non-function applied as if it were one."); `Unknown

let rec type_statement env (pos, statement) = 
    match statement with
        | EmptyStatement
        | Html _ -> env
              
        | Call id -> ignore (type_identifier env id); env

        (* TODO: check comments for annotations *)
        | Comment _ -> env

        | CompoundStatement ((While e), body) ->
              if unify `Boolean (type_expression env e) then
                  List.fold_left type_statement env body
              else
                  (error (sprintf "Non-boolean used in boolean context."); env)
              

        | SubCall (id, rvals) ->
              let functype = type_identifier env id in
              let argtypes = List.map (type_expression env) rvals in
              ignore (type_application env functype argtypes);
              env

let type_ast ast =
    List.fold_left type_statement builtins ast;
    print_string "Check complete.\n"

(*
	| BinaryOp of binary_operator * rvalue * rvalue
	| UnaryOp of unary_operator * rvalue

    | Identifier of identifier
	| Eval of rvalue	(** TODO: make this a standard lib function, instead of syntactic construct. *)

	| New of identifier

	| Null
	| Nothing
*)

(*
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
        
*)
