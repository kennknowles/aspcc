
(* Printing functions *)
open Printf
open AspAst

let trim_eol s =
    try
        ignore (String.index s '\n');
        String.sub s 0 ((String.length s) - 1)
    with
        | Not_found -> s
              (* Invalid_argument means the string has 0 length *)
        | Invalid_argument _ -> s

let indent indentation str =
    (Str.global_replace (Str.regexp "^") indentation (trim_eol str)) ^ "\n"

(* little helpers *)
let concat_map ?str func the_list =
	String.concat
		(match str with
		     | Some c -> c
		     | None -> "")
		(List.map func the_list)

let comma_map func the_list = concat_map ~str:", " func the_list

let string_of_binary_op = function
	| Concat -> "&"
	| Add -> "+"
	| Subtract -> "-"
	| Mult -> "*"
	| Div -> "/"
    | IntDiv -> "\\"
    | Equals -> "="
    | NotEquals -> "<>"
    | Is -> "is"
    | GreaterEqual -> ">="
    | LessEqual -> "<="
    | Greater -> ">"
    | Less -> "<"
    | Eqv -> "eqv"
    | Imp -> "imp"
    | Xor -> "xor"
    | Or -> "or"
    | And -> "and"
    | Exp -> "exp"
    | Mod -> "mod"

let string_of_unary_op = function
	| AspAst.Negative -> "-"
	| AspAst.Not -> "not"

(*============= vb_identifier ================*)
let rec string_of_id id =
	match id with
	    | AspAst.AtomicId s -> Symbol.to_string s
	    | AspAst.Dot (base,member) ->
		      sprintf "%s.%s" (string_of_id base) (Symbol.to_string member)
	    | AspAst.Indices (base,indices) ->
		      sprintf "%s( %s )"
		      (* or, if it is a function in the current scope "%s(%s)" *)
		      (string_of_id base)
		      (comma_map string_of_rvalue indices)

(*============= vb_rvalue ================*)
and string_of_rvalue rval =
	match rval with
        | Int (Bin, b) -> sprintf "&b(%li)" b
	    | AspAst.Int (AspAst.Dec, i) -> sprintf "%li" i
	    | AspAst.Int (AspAst.Hex, h) -> sprintf "&h%lx" h
	    | AspAst.Int (AspAst.Oct, o) -> sprintf "&o%lo" o
        | AspAst.Int (AspAst.Bin, b) -> sprintf "&b(%li)" b
	    | AspAst.Float f -> sprintf "%f" f
	    | AspAst.String s -> sprintf "\"%s\"" s  (* vbscript has nothing like PHP 
											        double-quote strings *)
	    | AspAst.Bool b -> if b then "true" else "false"

	    | AspAst.BinaryOp (op, v1, v2) -> sprintf "(%s %s %s)"
			  (string_of_rvalue v1) (string_of_binary_op op) (string_of_rvalue v2)
	          
	    | AspAst.UnaryOp (op, v) -> sprintf "(%s %s)"
		      (string_of_unary_op op) (string_of_rvalue v)

        | AspAst.Identifier identifier -> string_of_id identifier

        | New id -> sprintf "new %s" (string_of_id id)

        | Eval exp -> sprintf "eval( %s )" (string_of_rvalue exp)

        | Nothing -> "nothing"
        | Null -> "null"


(* for now, we just need really basic info for debugging/error msgs *)
and string_of_statement statement_body =
    try
	    match statement_body with
	        | AspAst.EmptyStatement -> "\n" (*"// empty statement \n" *)
	        | AspAst.Assignment (a_id, a_val) -> 
		          sprintf "%s = %s" (string_of_id a_id) (string_of_rvalue a_val)
                  
	        | AspAst.Call id -> "Call " ^ (string_of_id id)
                  
	        | AspAst.Class (name, statements) -> sprintf "class %s" (Symbol.to_string name)
	              
	        | AspAst.Comment c -> ""
                  
	        | AspAst.CompoundStatement (compound_type, statements) ->
		          (match compound_type with
		               | AspAst.For (var, start, finish, inc) -> sprintf
				             "for %s = %s to %s step %s"
				             (Symbol.to_string var) 
                             (string_of_rvalue start)
				             (string_of_rvalue finish)
				             (string_of_rvalue inc)

		               | AspAst.ForEach (name, collection) -> sprintf
                             "for each %s in %s)\n" 
                             (Symbol.to_string name)
                             (string_of_rvalue collection)

		               | AspAst.While expr -> sprintf "while %s " (string_of_rvalue expr)

		               | AspAst.DoWhile expr -> sprintf "do while %s" (string_of_rvalue expr)

		               | AspAst.With lval -> "")
                  
	        | AspAst.Const (name, rval) -> 
		          sprintf "const %s = %s" (Symbol.to_string name) (string_of_rvalue rval)

	        (* TODO: in class context, cannot assign this to array() *)
	        | AspAst.Dim id_list -> 
		          sprintf "dim %s" (comma_map string_of_id id_list)

	        | AspAst.Erase (var) -> sprintf "erase %s" (string_of_id var)

	        | AspAst.Execute (rval) -> sprintf "execute(%s)" (string_of_rvalue rval)

	        | AspAst.Exit target ->
		          (match target with
		               | `Function -> "exit function"
		               | `Sub -> "exit sub"
                       | `Property -> "exit property"
		               | _ -> "exit ...")

	        | AspAst.Function (name,params,statements) -> sprintf "function %s" (Symbol.to_string name)

	        | AspAst.If (predicate, then_block, else_block) ->
		          sprintf "if %s" (string_of_rvalue predicate)

            (*
	          | AspAst.MemberDef (access, default, memberstatement) ->
		    (* PHP doesn't have private/public *)
		      (match memberstatement with
		      | AspAst.MemberIdent id -> sprintf "var $%s;\n" (sprint_identifier id)
		      | AspAst.MemberFunction x | AspAst.MemberSub x -> sprint_function x
		      | AspAst.PropertyLet (name, params, statements) -> 
		      sprint_function (name ^ "_let", params, statements)
		      | AspAst.PropertyGet (name, params, statements) ->
		      sprint_function (name ^ "_get", params, statements)
		      | AspAst.PropertySet (name, params, statements) ->
		      sprint_function (name ^ "_set", params, statements))

	          | AspAst.OnErrorResumeNext | AspAst.OnErrorGotoZero ->
		      "// error handling? bah!\n"

	          | AspAst.OptionExplicit ->
		      "// option explicit?  i guess i could look it up, bleh\n"

	          | AspAst.Randomize ->
		      "list ($r_usec, $r_sec) = explode(' ', microtime());\n"
		      ^ "srand((float)$r_sec + (($float) $r_usec * 100000));\n"

	          | AspAst.ReDim (preserve, id) ->
		      sprint_dim context preserve id

	          | AspAst.Select (rval, options) ->
		      sprint_select context (rval, options)
		      
	        (* TODO: here is where we  can track the class type... i  wish i knew
		      some type-checking theory *)
            *)
	        | AspAst.Set (id, rval) ->
		          sprintf "set %s = %s" (string_of_id id) (string_of_rvalue rval)

	        | AspAst.Ssi (k,f) -> sprintf "<!-- #include %s=\"%s\" -->" k f

            (*
	          | AspAst.Sub func_def -> sprint_function func_def 
            *)
	        | AspAst.SubCall (id, rvals) -> 
		          sprintf "%s %s" (string_of_id id) (comma_map string_of_rvalue rvals)

                  
    with
        | Match_failure _ -> "Unimplemented Vb-dump"
