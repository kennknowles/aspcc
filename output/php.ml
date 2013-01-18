

(* Printing functions *)
open Printf


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

(*===========================================================================*)
(* For managing a symbol table and class definitions                         *)
(*===========================================================================*)

(* TODO: this could be an entirely separate module for managing VB's symbol table *)

exception SymbolFound

type symbol_variety =
    | Sub of AspAst.function_def
    | Function of AspAst.function_def
    | Variable of string * (string option) (* the string option knows what 
												type the var is, for OO lookup *)
    | Class of AspAst.class_def
	| Const of AspAst.constant_definition
	| Dummy of unit

type statement_context =
	| ToplevelContext
	| ClassContext
	| FunctionContext of string

let symbol_table = Hashtbl.create 53

let scope_stack = Stack.create ()

let add_symbol name definition =
	Hashtbl.add symbol_table (String.lowercase name) definition;
    Hashtbl.add (Stack.top scope_stack) (String.lowercase name) ()

(*let find_symbol_in name sym_table =
	try
		let result = Hashtbl.find sym_table (String.lowercase name) in
		Some result
	with
		Not_found -> None*)

let find_symbol name =
	Hashtbl.find symbol_table name

let push_scope () =
    Stack.push (Hashtbl.create 53) scope_stack

let pop_scope () =
	Hashtbl.iter (fun key value -> Hashtbl.remove symbol_table key) (Stack.pop scope_stack)

(* Add all the built-in functions and intrinsic objects*)

(* Todo: add param info... add OO member symbol tables... add past the letter 'a' *)


let _ =
    push_scope ();
    List.iter (fun (x,y) -> add_symbol x y)
        [   "Abs", Function ("Abs", [], []);
			"Array", Function ("Array", [], []); 
			"Asc", Function ("Asc", [], []);
			"Atn", Function ("Atn", [], []);
			"CBool", Function ("CBool", [], []);
			"CByte", Function ("CByte", [], []);
			"CCur", Function ("CCur", [], []);
			"CDate", Function ("CDate", [], []);
			"CDbl", Function ("CDbl", [], []);
			"Chr", Function ("Chr", [], []);
			"CInt", Function ("CInt", [], []);
			"CLng", Function ("CLng", [], []);
			"Cos", Function ("Cos", [], [])		];
    
    (* an AspAst.Ident in the symbol table represents a variable *)
    List.iter 
		(fun (x, y) -> 
			add_symbol x (Variable (x, None)))
        
		[   "Response", "AspResponse";
			"Request", "AspRequest";
			"Server", "AspServer";
			"Session", "AspSession"  ]



(* little helpers *)

(* see if the current thing is registered as something other than a variable,
	and thus needs no dollar sign *)
let is_variable symbol =
	try (
		match find_symbol symbol with
			| Variable _ -> true
			| _ -> false
	) 	
	with
	| Not_found -> true (* not quite, but could be an undeclared var *)


let concat_map ?str func the_list =
	String.concat
		(match str with
		| Some c -> c
		| None -> "")
		(List.map func the_list)

let comma_map func the_list = concat_map ~str:", " func the_list
let arrow_map func the_list = concat_map ~str:"->" func the_list


(*====== For tracking equivalencies for PHP =====*)
type php_mapping_function = 
	(AspAst.identifier -> string)

let equivalency_table = Hashtbl.create 53

let get_php_equivalent vb_id =
	Hashtbl.find equivalency_table (String.lowercase vb_id)

let add_equivalency vb_func sprint_func =
	Hashtbl.add equivalency_table (String.lowercase vb_func) sprint_func

(* the adding of equivalent mappings occurs below, for declaration order convenience *)


(*===========================================================================*)
(* For outputing the PHP                                                     *)
(*===========================================================================*)

(*============= vb_identifier ================*)
(* TODO: lookup of equivalencies *)
let rec sprint_identifier ?(top : bool = true) id =
(*	if top then
		let format_func = get_php_equivalent (id_collapse id)
*)
	match id with
	| AspAst.AtomicId s -> (if is_variable s then "$" else "") ^ s
	| AspAst.Dot (base,member) ->
		sprintf "%s->%s" (sprint_identifier base) member
	| AspAst.Indices (base,indices) ->
		sprintf "%s[%s]"
		(* or, if it is a function in the current scope "%s(%s)" *)
		(sprint_identifier base)
		(comma_map sprint_rvalue indices)

(*============= vb_rvalue ================*)
and sprint_rvalue rval =
	match rval with
	| AspAst.Int (AspAst.Dec, i) -> sprintf "%li" i
	| AspAst.Int (AspAst.Hex, h) -> sprintf "%lx" h
	| AspAst.Int (AspAst.Oct, o) -> sprintf "%lo" o
	| AspAst.Float f -> sprintf "%f" f
	| AspAst.String s -> sprintf "'%s'" s  (* vbscript has nothing like PHP 
											double-quote strings *)
	| AspAst.Bool b -> if b then "TRUE" else "FALSE"
(*
	Re-implement these later - they need to be converted to the new AST layout 

	| AspAst.Concat (v1, v2) -> sprintf "(%s . %s)"
								(sprint_rvalue v1) (sprint_rvalue v2)

	| AspAst.Negative i -> sprintf "-%s" (sprint_rvalue i)
	| AspAst.Add (v1, v2) -> sprintf "(%s + %s)" 
							(sprint_rvalue v1) (sprint_rvalue v2)
	| AspAst.Subtract (v1, v2) -> sprintf "(%s - %s)" 
							(sprint_rvalue v1) (sprint_rvalue v2)
	| AspAst.Mult (v1, v2) -> sprintf "(%s * %s)" 
							(sprint_rvalue v1) (sprint_rvalue v2)
	| AspAst.IntDiv (v1, v2) -> sprintf "(%s / %s)" 
							(sprint_rvalue v1) (sprint_rvalue v2)
	| AspAst.Div (v1, v2) -> sprintf "((1.0 * %s) / %s)" 
							(sprint_rvalue v1) (sprint_rvalue v2)
	| AspAst.Exp (v1, v2) -> sprintf "(%s ^ %s)" 
							(sprint_rvalue v1) (sprint_rvalue v2)


	| AspAst.Not n -> sprintf "!(%s)" (sprint_rvalue n)
	(* TODO: does not support bitwise operators *)
	| AspAst.And (v1, v2) -> sprintf "(%s && %s)"
							(sprint_rvalue v1) (sprint_rvalue v2)
	| AspAst.Or (v1, v2) -> sprintf "(%s || %s)"
							(sprint_rvalue v1) (sprint_rvalue v2)
	| AspAst.Xor (v1, v2) -> sprintf "(%s xor %s)"
							(sprint_rvalue v1) (sprint_rvalue v2)
	(* convert a->b to (not a or b) *)
	| AspAst.Imp (v1, v2) -> sprintf "(!%s || %s)"
							(sprint_rvalue v1) (sprint_rvalue v2)
	| AspAst.Eqv (v1, v2) -> sprintf "(%s == %s)"
							(sprint_rvalue v1) (sprint_rvalue v2)

	| AspAst.Equals (v1, v2) -> sprintf "(%s == %s)"
							(sprint_rvalue v1) (sprint_rvalue v2)
	| AspAst.NotEquals (v1, v2) -> sprintf "(%s != %s)"
							(sprint_rvalue v1) (sprint_rvalue v2)
	| AspAst.LessEqual (v1, v2) -> sprintf "(%s <= %s)"
							(sprint_rvalue v1) (sprint_rvalue v2)
	| AspAst.GreaterEqual (v1, v2) -> sprintf "(%s >= %s)"
							(sprint_rvalue v1) (sprint_rvalue v2)
	| AspAst.Less (v1, v2) -> sprintf "(%s < %s)"
							(sprint_rvalue v1) (sprint_rvalue v2)
	| AspAst.Greater (v1, v2) -> sprintf "(%s > %s)"
							(sprint_rvalue v1) (sprint_rvalue v2) *)
    | AspAst.Identifier identifier -> sprint_identifier identifier

	| _ -> "/* Unknown expression */"



(* since declaring member vars to be arrays must take place in the constructor, we need a function
	to scan a statement and output the constructor shit *)

(* TODO: support preserving... probably by an extremely complex PHP function *)
let rec sprint_dim_indices size_list =
	match size_list with
	| [] -> "''"
	| size::rest -> 
		sprintf "array_pad(array(), %s, %s)"
			(sprint_rvalue size)
			(sprint_dim_indices rest)

let sprint_dim_tail id =
	match id with
	| AspAst.Indices (base, []) -> 
		sprintf "%s = array();\n" (sprint_identifier base)
	| AspAst.Indices (base, size) -> 
		sprintf "%s = %s;\n" 
			(sprint_identifier base) 
			(sprint_dim_indices size)
	| _ -> ""

let sprint_index_free_id id =
	match id with
	| AspAst.Indices (base, indices) ->
		sprint_identifier base

	| x -> sprint_identifier x

let sprint_constructor_info (line_num, statement) =
	match statement with
(*	| AspAst.MemberDef (_, _,(AspAst.MemberIdent id))
	| AspAst.Dim id -> 
		sprint_dim_tail id *)
	| _ -> ""


(*========= vb_function =========*)
let rec sprint_function ((name, params, statements) : AspAst.function_def) =
	(* TODO: when assigning to the function name, set $returnvalue, and always
		return it *)
    ""

(*	sprintf "function %s(%s)\n{\n%s}\n" name (String.concat "," params)
		(indent "\t" ("$returnvalue = '';\n" ^
					(concat_map 
						(sprint_statement ~context:(FunctionContext name)) 
						statements) ^
					"return ( $returnvalue );\n"))*)

(*============== vb_assignment ================ *)
and sprint_assignment context (id, rval) =
	(match context, id with

	(* This crazy match checks if the variable being assigned is
		the function of the current context *)
	| 	(FunctionContext funcname), 
		(AspAst.AtomicId varname) when (funcname=varname) 
		-> 
			"$returnvalue"

	| _, _ -> (sprint_identifier id))

	^ " = " ^ (sprint_rvalue rval) ^ ";\n";

and sprint_dim context preserve id =
	match context with
	| ClassContext -> 
		sprintf "var %s;\n" (sprint_index_free_id id)

	| _ -> sprint_dim_tail id

and sprint_select context (rval, options) =
	sprintf "switch(%s)\n{\n%s}"
		(sprint_rvalue rval)
		(indent "\t"
			(concat_map (sprint_select_item context) options))

and sprint_select_item context (item, statements) =
	(match item with
	| AspAst.SelectValue rvals ->  "case " ^ 
								(concat_map ~str:":\ncase " sprint_rvalue rvals) 
								^ ":\n"
	| AspAst.SelectElse -> "default:\n")
	^
	(indent "\t" (	(concat_map (sprint_statement ~context:context) statements)
					^ "break;\n"))

(*	context is probably needed: just an (AspAst.vb_compound_statement Stack.t), and is needed
	for With since php has no such thing, and Exit since we might need a numeric argument to break *)
and sprint_statement ?(context = ToplevelContext) (line_num, statement) =
	match statement with
	| AspAst.EmptyStatement -> "\n" (*"// empty statement \n" *)
	| AspAst.Assignment (a_id, a_val) -> 
		sprint_assignment context (a_id, a_val)

	| AspAst.Call id -> (sprint_identifier id)  ^ ";\n"

	| AspAst.Class (name, statements) ->
		sprintf "class %s { \n%s }\n"
			name
			(indent "\t" (
				(String.concat 
					"" 
					(List.map 
						(sprint_statement ~context:ClassContext) 
						statements)) ^
				(sprintf "function %s()\n{\n%s}\n" name
					(indent "\t" 
						(String.concat "" 
							(List.map sprint_constructor_info statements))))))
	| AspAst.Comment c ->
		"// " ^ c

	| AspAst.CompoundStatement (compound_type, statements) ->
		(match compound_type with
		(*	unsafe use of != instead of < or > because step can be negative 
			a safe workaround is to assign STEP to a var, and then have an overly complex
			condition that is > if it is negative or < if it is positive... before that happens we
			should do a match on inc to see if it is simple and we can determine it at compile time *)
		| AspAst.For (var, start, finish, inc) ->
			sprintf
				"for(%s = %s; %s != %s; %s += %s)\n"
				var (sprint_rvalue start)
				var (sprint_rvalue finish)
				var (sprint_rvalue inc)

		| AspAst.ForEach (name, collection) ->
			sprintf "foreach(%s as $%s)\n" (sprint_rvalue collection) name

		| AspAst.While expr ->
			sprintf "while(%s)\n" (sprint_rvalue expr)

		| AspAst.With lval -> ""

		) ^
		"{\n" ^
			(indent "\t" (String.concat "" (List.map sprint_statement statements))) ^
		"}\n"

	| AspAst.Const (name, rval) ->
		sprintf "define(\"%s\", %s);\n" name (sprint_rvalue rval)

	(* TODO: in class context, cannot assign this to array() *)
(*	| AspAst.Dim id -> 
		sprint_dim context false id *)

	| AspAst.Erase (var) ->
		(sprint_identifier var) ^ " = '';\n"

	| AspAst.Execute (rval) ->
		"// WARNING: the string passed to this eval has *not* been converted!\n" ^
		"eval(" ^ (sprint_rvalue rval) ^ ");\n"

	| AspAst.Exit target ->
		(match target with
		| `Function -> "return ( $returnvalue );\n"
		| `Sub -> "return;\n"
		(* TODO: break with a number to bust out of many levels deep *)
		| _ -> "break;\n")

	| AspAst.Function func_def -> sprint_function func_def 
		
	| AspAst.If (predicate, then_block, else_block) ->
		sprintf "if ( %s ) \n{\n%s}\nelse\n{\n%s}\n"
			(sprint_rvalue predicate)
			(indent "\t" (String.concat "" (List.map sprint_statement then_block)))
			(indent "\t" (String.concat "" (List.map sprint_statement else_block)))

	| AspAst.MemberDef (access, default, memberstatement) ->
		(* PHP doesn't have private/public *)
		(match memberstatement with
	(*	| AspAst.MemberIdent id_list -> sprintf "var $%s;\n" (sprint_identifier id) *)
		| AspAst.MemberFunction x | AspAst.MemberSub x -> sprint_function x
		| AspAst.PropertyLet (name, params, statements) -> 
			sprint_function (name ^ "_let", params, statements)
		| AspAst.PropertyGet (name, params, statements) ->
			sprint_function (name ^ "_get", params, statements)
		| AspAst.PropertySet (name, params, statements) ->
			sprint_function (name ^ "_set", params, statements))

(*	| AspAst.OnErrorResumeNext | AspAst.OnErrorGotoZero ->
		"// error handling? bah!\n"
*)
	| AspAst.OptionExplicit ->
		"// option explicit?  i guess i could look it up, bleh\n"

	| AspAst.Randomize ->
		"list ($r_usec, $r_sec) = explode(' ', microtime());\n"
		^ "srand((float)$r_sec + (($float) $r_usec * 100000));\n"

	| AspAst.ReDim (preserve, id_list) ->
		"" (*List.map (sprint_dim context preserve) id_list*)

	| AspAst.Select (rval, options) ->
		sprint_select context (rval, options)
		
	(* TODO: here is where we  can track the class type... i  wish i knew
		some type-checking theory *)
	| AspAst.Set (id, rval) ->
		sprint_assignment context (id, rval)

	| AspAst.Sub func_def -> sprint_function func_def 

	| AspAst.SubCall (id, rvals) -> 
		sprint_identifier (AspAst.Indices (id, rvals)) ^ ";\n"

	(*| _ -> "// Encountered unknown statement\n"*)

let sprint_page page =
""(*	String.concat
		""
		(List.map
			(function 
				| AspAst.Html html -> html 
				| AspAst.VbScript vb -> 
					"<?" ^ 
						(indent "\t" (String.concat "" 
							(List.map sprint_statement vb)))
					^ "\n?>")
			page)*)

let print_page page =
	print_string (sprint_page page)




(*========================================================================*)
(*================== Addition of existing mappings =======================*)
(*========================================================================*)

(* note: 'direct_mapping' and 'implemented' do the same thing now, but
this may not always be the case, so no merging *)

let direct_mapping php_func =
	(fun (_, rvals) ->
		sprintf "%s(%s)"
			php_func
			(comma_map sprint_rvalue rvals))

(* the comma_map in this should be unneccessary since we can only
convert a single value! *)
let map_to_cast php_type =
	(fun (_, rvals) ->
		sprintf "((%s) %s)"
			php_type
			(comma_map sprint_rvalue rvals))

let map_to_literal php_code =
	(fun (_, rvals) -> php_code)

let implemented php_func =
	(fun (_, rvals) ->
		sprintf "%s(%s)"
			php_func
			(comma_map sprint_rvalue rvals))

let format_dateserial (_, rvals) =
	match rvals with
	| year::month::day::_ ->
		sprintf "mktime(0, 0, 0, %s, %s, %s)"
			(sprint_rvalue month)
			(sprint_rvalue day)
			(sprint_rvalue year)
	| _ -> "" (* TODO: raise an exception here *)

(* note: this get_date is not a builtin, but a wrapper
	because php cannot parse this: getdate(..)['...'] *)

let format_date datepart (_, rvals) =
	sprintf "date('%s', datevalue(%s))"
		datepart
		(comma_map sprint_rvalue rvals)

let format_instr (_, rvals) =
	match rvals with
	| haystack::needle::[] ->
		sprintf "instr(%s, %s)"
			(sprint_rvalue haystack)
			(sprint_rvalue needle)

	| start::haystack::needle::[] ->
		sprintf "instr(%s, %s, %s)"
			(sprint_rvalue haystack)
			(sprint_rvalue needle)
			(sprint_rvalue start)

	| start::haystack::needle::compare::_ ->
		sprintf "instr(%s, %s, %s, %s)"
			(sprint_rvalue haystack)
			(sprint_rvalue needle)
			(sprint_rvalue start)
			(sprint_rvalue compare)

	| _ -> "" (* TODO: raise an exception here *)
		

(*============ for converting vb function calls to php ==========*)

(*	Comprehensive list of VBscript functions.
	There are four categories of functions:

	* Direct mappings
	* Functions implemented in PHP in asp_implementations.php 
	* Functions that make no sense
	* Functions that are unsupported at this time
*)
	
(* TODO: make implementations for casts, which do the checking the
VbScript functions would... though they error all the time in horrendous
ways :( *)

(* btw 0 is false, true is -1, use default is -2 *)
let _ =
	List.iter (fun (x, y) -> add_equivalency x y)
		[	
	(* ============== BUILTIN FUNCTIONS ============== *)
			"abs", (direct_mapping "abs");
			"array", (direct_mapping "array");
			"asc", (direct_mapping "ord");
			"atn", (direct_mapping "atan");
			"cbool", (map_to_cast "bool");
			"cbyte", (map_to_cast "int");
			"ccur", (map_to_cast "double");
			"cdate", (implemented "datevalue");
			"cdbl", (map_to_cast "double");
			"chr", (direct_mapping "chr");
			"cint", (map_to_cast "int");
			"clng", (map_to_cast "int");
			"cos", (direct_mapping "cos");
			(* "createobject"  not supported, it would be useless anyhow *)
			"csng", (map_to_cast "float");
			"cstr", (map_to_cast "string");
			"date", (map_to_literal "date('m/d/Y')");
			"dateadd", (implemented "dateadd");
			"datediff", (implemented "datediff");
			"datepart", (implemented "datepart");
			"dateserial", format_dateserial;
			"datevalue", (implemented "datevalue");
			"day", (format_date "d");
		(* Eval  sux *)
			"exp", (direct_mapping "exp");
			"filter", (implemented "filter");
			"fix", (implemented "fix");
			"formatcurrency", (fun (id,rvals) -> 
								"$" ^ (implemented "formatnumber" (id,rvals)));
			"formatdatetime", (implemented "formatdatetime");
			"formatnumber", (implemented "formatnumber");
			"formatpercent", (implemented "formatpercent");
		(* GetLocale returns some unique id number for locle LCID*)
		(* GetObject this is some fucked up thing  *)
		(* GetRef dunno wtfg *)
			"hex", (direct_mapping "dechex");
			"hour", (format_date "H");
		(* "inputbox" client side only *)
			"instr", format_instr;
			"instrrev", (implemented "instrrev");
			"int", (direct_mapping "floor");
			"isarray", (direct_mapping "is_array");
			"isdate", (implemented "isdate");
			"isempty", (fun (id,rvals) ->
							sprintf "!%s" (sprint_rvalue (List.hd rvals)));
			"isnull", (fun (id,rvals) ->
							sprintf "!%s" (sprint_rvalue (List.hd rvals)));
			"isnumeric", (direct_mapping "is_numeric"); (* NOT is_int *)
			"isobject", (direct_mapping "is_object");
			"join", (fun (id,rvals) ->
						match rvals with
						| val1::val2::[] -> sprintf "join(%s, %s)" 
											(sprint_rvalue val2) 
											(sprint_rvalue val1)
						| _ -> "" (* TODO: raise exception *));
			"lbound", (map_to_literal "0");
			"lcase", (direct_mapping "strtolower");
			"left", (implemented "left");
			"len", (direct_mapping "strlen");
		(* LoadPicture - nonsense *)
			"log", (direct_mapping "log");
			"ltrim", (direct_mapping "ltrim");
			"mid", (direct_mapping "substr");
			"minute", (format_date "i");
			"month", (format_date "m");
			"monthname", (implemented "monthname");
		(* MsgBox - nonsense *)
			"now", (map_to_literal "date('D M d H:i:s Y')");
			"oct", (direct_mapping "decoct"); 
		(* Replace(string,find,replace,start,count,compare) *)
		(* RGB - who cares *)
			"rght", (implemented "right");
			"rnd", (fun (id,rvals) -> "(rand()/(float)getrandmax())");
			"round", (direct_mapping "round");
			"rtrim", (direct_mapping "rtrim");
		(* ScriptEngine - returns language, i.e. vbscript *)
		(* ScriptEngineBuildVersion   none of these maatter *)
		(* ScriptEngineMajorVersion *)
		(* ScriptEngineMinorVersion *)
			"second", (format_date "s");
		(* SetLocale more LCID stuff *)
			"sgn", (fun (id,rvals) -> 
						let v = (sprint_rvalue (List.hd rvals)) in
						sprintf "(abs(%s)/%s)" v v);
		(* Sgn - the sign of a number *)
			"sin", (direct_mapping "sin"); 
			"space", (fun (id,rvals) -> sprintf "str_pad('', %s)"
											(sprint_rvalue (List.hd rvals)));
		(* Split(expression, delim, count, compare) *)
			"sqr", (fun (id,rvals) -> 
						let expr = (sprint_rvalue (List.hd rvals)) in
						sprintf "(%s * %s)" expr expr);
		(* StrComp(string1,string2,compare) - strcmp *)
			"string", (fun (id,rvals) -> sprintf "str_pad('', %s, %s)"
										(sprint_rvalue (List.hd rvals))
										(sprint_rvalue (List.nth rvals 1)));
		(* String(num,char) - use str_pad *)
			"strreverse", (direct_mapping "strrev");
			"tan", (direct_mapping "tan");
			"time", (map_to_literal "date('h:i:s A')");
		(* Timer - number of seconds to seven decimal places since midnight *)
			"timeserial", (direct_mapping "mktime");
		(* TimeValue(aanything) *)
			"trim", (direct_mapping "trim");
			"ubound", (fun (id,rvals) -> sprintf "(count(%s)-1)"
										(sprint_rvalue (List.hd rvals)));
			"ucase", (direct_mapping "strtoupper");
		(* VarType - something like gettype *)
			"weekday", (fun (id, rvals) -> 
							sprintf "(%s + 1)"
							(format_date "w" (id,rvals)));

			"weekdayname", (implemented "weekdayname");
			"year", (format_date "Y");

	(* ================ RESPONSE ====================== *)
			"response.write", (direct_mapping "echo");
			"response.end", (direct_mapping "exit");
	
	(* ================ SERVER ====================== *)
			"server.execute", (direct_mapping "require");
			"transfer", (fun (id,rvals) ->
							(direct_mapping "require" (id,rvals)) ^
							";\nexit();\n")
	]
						
(*
	================ XML ==================

	set zzz = Server.CreateObject("Microsoft.XMLDOM")
		-> nothing
	
	zzz.load(filepath) -> $zzz = domxml_open_file(filepath);

	=== exact method matches:
	
	zzz.GetElementsByTagName(xxx)		$zzz->get_elements_by_tagname(xxx)
	zzz.ChildNodes						$zzz->child_nodes()
	zzz.NodeName						$zzz->node_name()

	== near matches
	zzz.GetAttribute(xxx)		$zzz->attributes()[xxx]
	zzz.Xml						$zzz->dump_node()
*)

(*
	================= intrinsics ================
	Application:
		not particularly useful

	Request:
		supported by PHP's $_REQUEST[...], though a wrapper for
		safety may be a good idea.  Also can use %
		
		.Form				$_POST
		.QueryString		$_GET
		.ServerVariables	$_SERVER
			PATH_INFO		"http://$_SERVER[SERVER_NAME]$_SERVER[PHP_SELF]"
							also $_SERVER[PATH_INFO] though I'm not sure it
							is the same.. hopefully!

	Response:
		.ContentType		header("Content-type: " . xxx);
		.End				exit();
		.Write				echo( xxx );
		.Redirect			header("Location: " . xxx);
			more specifically
				header("Location: http://"	. $_SERVER['HTTP_HOST']
											. dirname($_SERVER['PHP_SELF'])
											. "/" . xxx );
	Server:
		.ScriptTimeout = seconds		set_time_limit(seconds)
		.CreateObject( ... )			either 'new' or special case
		.Execute( ... )					include(...) for suck
										require_once(...) for good
										(you can mix and match include/require
										+ once/many)

		.HtmlEncode( xxx )				htmlspecialchars(xxx)
		.MapPath(xxx)					the way the path is generated it will
										already be mapped, i hope
		.UrlEncode(xxx)					urlencode(xxx)

	Session:
		.SessionID				session_id()
		.Timeout = xxx			session_set_cookie_params(lifetime [, ...])
		.Contents("xxx")		$_SESSION['xxx']
		.Abandon				session_destroy()

		.OnStart .OnEnd			can hack it in with a require_once script
								that populates it if and only if it isn't
								already there

*)
