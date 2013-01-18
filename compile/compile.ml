open Opcode
open Printf 

module SymbolMap = Map.Make(Symbol)
module SymbolSet = Set.Make(Symbol)

type var_location = Global of int | Local of int

(* As a challenge to myself, I want to do this with no side effects *)
type compile_time_info = {
	stack_depth : int;
	label_counter : int;
	current_func : string;
	variables : var_location SymbolMap.t;
	functions : string SymbolMap.t;

	(* Labels to jump to for vbscript "exit" statements *)
	exitwhile : string;
	exitdo : string;
	exitfunction : string;
	exitfor : string;
	exitsub : string;
	exitproperty : string;
	exitwith : string
}



let empty_info = {
	stack_depth = -1;
	label_counter = 0;
	current_func = "";
	variables = SymbolMap.empty;
	functions = SymbolMap.empty;
	
	exitwhile = "";
	exitdo = "";
	exitfunction = "";
	exitfor = "";
	exitsub = "";
	exitproperty = "";
	exitwith = ""
}

let string_of_binary_op = function
	| AspAst.Mult -> "*"
	| AspAst.Add -> "+"
	| AspAst.Greater -> ">"
	| AspAst.Less -> "<"

let push info = {info with stack_depth = info.stack_depth + 1}
let pop info = {info with stack_depth = info.stack_depth - 1}

let make_label info =
	let x = "L" ^ (string_of_int info.label_counter) in
	{info with label_counter = info.label_counter + 1}, x

(*	A little module with wrappers for each opcode, so we can separate the analysis bookkeeping from
	the code generation *)
module Op = struct
	let let_of_location = function
		| Global i -> LetGlobal i
		| Local i -> LetLocal i
	          
	let get_of_location = function
		| Global i -> GetGlobal i
		| Local i -> GetLocal i

	let lookup_get info name = get_of_location (SymbolMap.find name info.variables)
	let lookup_let info name = let_of_location (SymbolMap.find name info.variables)

	let assign_let info name =
		pop info, 
		[(Comment (sprintf "Assignment to %s" (Symbol.to_string name), 
			       Some (lookup_let info name)))]

	let const info value =
		push info, [Const value]

	let get info name =
		push info,
		[(Comment (sprintf "Retrieval of %s" (Symbol.to_string name), 
			       Some (lookup_get info name)))]
		
	let alloc info name =
		let newdepth = info.stack_depth + 1 in
		let location = if info.current_func = "" then Global newdepth else Local newdepth in
		{info with
			 stack_depth = newdepth;
			 variables = SymbolMap.add name location info.variables
		},
		[Comment (sprintf "Allocate %s" (Symbol.to_string name), Some (Const VbTypes.Empty))]
	    
end

(*=================================================================================*)
(* Code Generation                                                                 *)
(*=================================================================================*)

(* This purely applicative function generates code for a sequence of AST nodes *)
let rec fold_code_gen gen_func info astnodes =
	List.fold_left
		(fun (incoming_info, opcodes) this_node ->
			 let outgoing_info, code = gen_func incoming_info this_node in
			 outgoing_info, opcodes @ code)
		(info, [])
		astnodes

let rec identifier ?(return_object=false) ?(property_context=`Get) info id =
	match id with
	    | AspAst.AtomicId a -> (
		      (*	In the case of a variable or object member, it'll be in the stack at a known depth *)
		      try Op.get info a
		      with Not_found -> 

		          (*	Otherwise try to apply it as a zero-parameter function *)
		          try 
			          let info, ops = Op.const info (VbTypes.String (SymbolMap.find a info.functions)) in
			          info, ops @ [Apply 0]
		          with Not_found ->
		              
		              (* Who the hell knows! *)
		              info, []
		  )

	    | AspAst.Dot (id, s) -> 
			  (* Deref takes a string argument even though it doesn't have to 
				 - it could push an Atom *)
			  let info, ident = (identifier info id) in
			  info, ident @ [Deref (Symbol.to_string s)]

	    | AspAst.Indices (id, rvals) ->
		      let info, ops = fold_code_gen expression info rvals in
		      let info, ident = identifier info id in
		      info, ops @ ident @ [Apply (List.length rvals)]

(*	We now know that the previous code analysis has allocated space for anything that needs it *)
and let_identifier info id =
	match id with
	    | AspAst.AtomicId name -> (
	          (* 1 - Variables of know address. Including:
		         Global variables, not referenced by depth TODO!!!
		         Return value of current function is at depth zero relative to frame pointer
		         Object members are omitted for now.*)
	          try Op.assign_let info name
	          with Not_found -> info, [Comment (">> Unhandled let <<", None)])

(* 2 - local variable of current object
(*		NOTE: make two passes over objects, gathering their local vars, or
   you will hate life!!!

   we catch the exception because o#property will return it *)
   [Const (VbTypes.String name)]

   match info.this with
   | Some classname -> (o#property ~action:Let name)
   | None -> raise (Symbol_not_found "No 'this' object")*)

(* 3 - execute a default 'property let' on 'this' object TODO *)

(* 4 -	evaluate like a function applied to zero arguments, this will
   also create it if it doesn't exist *)
(*	try info, [Addr (SymbolMap.find name info.functions); LetDepth 0]
	with Not_found ->
(*	If a variable isn't declared, then we know option explicit is set or something funny is going on,
	so we should fail *)
	raise (Failure (sprintf
	"Use of undeclared variable '%s'(if option_explicit isn't set, then this is a compiler bug)"
	name))

	)*)
(* *)

	          
(* Thank god we know set/let at compile time! *)
and expression ?(return_object=false) info expr =
	match expr with
	    | AspAst.Int (_, i) -> Op.const info (VbTypes.Int i)
	    | AspAst.Float f -> Op.const info (VbTypes.Float f)
	    | AspAst.String s -> Op.const info (VbTypes.String s)
	    | AspAst.Bool s -> Op.const info (VbTypes.Bool s)

	    | AspAst.BinaryOp (op, v1, v2) -> 
		      let info, e1 = expression info v1 in
		      let info, e2 = expression info v2 in
		      pop info, List.flatten 
			      [
				      e1;
				      e2;
				      [Const (VbTypes.String (string_of_binary_op op)); Apply 2]
			      ]

	    | AspAst.Identifier id -> identifier ~return_object info id


let rec fold_statements info statements =
	List.fold_left
		(fun (incoming_info, opcodes) this_statement ->
			 let outgoing_info, code = statement incoming_info this_statement in
			 outgoing_info, opcodes @ code)
		(info, [])
		statements

and compound_statement info compound_type body =
	match compound_type with
	    | AspAst.For (var, start, finish, inc) ->
		      let info, forbegin = make_label info in
		      let info, forend = make_label info in

		      (* Assigning the first value *)
		      let info, start_ops = expression info start in
		      let info, let_ops = Op.assign_let info var in
		      
		      (* Evaluating the end condition *)
		      let info, finish_ops = expression info finish in
		      let info, get_opts = Op.get info var in
		      let info, equals_ops = Op.const info (VbTypes.String "=") in

		      (* Incrementing *)
		      let info, inc_ops = expression info inc in

		      let info, body_ops = fold_statements info body in
		      info, List.flatten
			      [
				      start_ops;
				      let_ops;
				      [Label forbegin];
				      body_ops;

				      finish_ops;
				      get_opts;
				      equals_ops;
				      [Apply 2];
				      [BranchIf forend];
				      
				      inc_ops;
				      let_ops;
				      [Branch forbegin];
				      [Label forend]
			      ]

	    | AspAst.While expr | AspAst.DoWhile expr ->
		      let info, whilebegin = make_label info in
		      let info, whileend = make_label info in
		      let info, exp_ops = expression info expr in
		      let info, body_ops = fold_statements info body in
		      info, List.flatten
			      [
				      [Label whilebegin];
				      exp_ops;
				      [BranchIfNot whileend];
				      body_ops;
				      [Branch whilebegin];
				      [Label whileend];
			      ]
				  
		          

and statement info (position, this_statement) =
	(* TODO: re-enabled positions *)
	(*Opcode.Position position ::*)
	
	match this_statement with
	    | AspAst.Assignment (a_id, a_val) -> 
		      let info, expr = expression info a_val in
		      let info, id = let_identifier info a_id in
		      info, (expr @ id)

        (*	| AspAst.Call id -> 
		    let sub_id, rvals = match id with
			| AspAst.Indices (func_id, rvals) -> 
			func_id, List.map (expression info) rvals
			| _ -> id, []
		    in
		    List.flatten 
			[
			List.flatten rvals;
			identifier info sub_id;
			[Apply (List.length rvals)]
			]*)

	    | AspAst.Comment _ -> info, []

	    | AspAst.CompoundStatement (compound_intro, body) ->
		      compound_statement info compound_intro body

	    (*	TODO: analyze the dim to see if it is an array dim... all dimming could be
		    compile time, -- in MS, they probably stack-allocate un-resizable arrays, and use
		    some magic type for resizable ones... I will just use magic for both *)
	    | AspAst.Dim ids -> info, []

	    | AspAst.EmptyStatement -> info, []

	    (*	The registers are managed by the runtime, not the opcodes, so there isn't
		    a saving convention *)
	    | AspAst.Function (name, params, body) ->
		      let beginlabel = "_function_" ^ (Symbol.to_string name) in
		      let endlabel = "_end_" ^ (Symbol.to_string name) in

		      (* Allocate a place for the function return value *)
		      let local_info = 
			      {info with
				       stack_depth = (List.length params);
				       variables = fst 
					                   (List.fold_left
						                    (fun (mapping, index) (_, var) -> 
							                     SymbolMap.add var (Local index) mapping, index + 1)
						                    (info.variables, 0)
						                    ((`ByVal, name) :: params))
			      }
		      in
	          
		      let _, ops = fold_statements local_info body in
			  (* Add the function to our inventory *)
			  {info with functions = SymbolMap.add name beginlabel info.functions},
			  (* Because functions can be inline with code, we branch over them *)
			  List.flatten
				  [
					  [Branch endlabel];
					  [Label beginlabel];
					  ops;
					  [Label endlabel]
				  ]

	    | AspAst.Html html -> info, [Const (VbTypes.String html); Write]

	    | AspAst.If (pred, thencode, elsecode) -> 
		      let info, thenlabel = make_label info in
		      let info, endlabel = make_label info in
		      let info, pred = expression info pred in
		      let info, thenops = fold_statements info thencode in
		      let info, elseops = fold_statements info elsecode in
		      info, List.flatten 
			      [
				      pred;
				      [BranchIf thenlabel];
				      elseops;
				      [Branch endlabel];
				      [Label thenlabel];
				      thenops;
				      [Label endlabel];
			      ]

	    | AspAst.InlineExpr v -> 
		      let info, expr = (expression info v) in
		      info, expr @ [Write]



(*=================================================================================*)
(* Analysis                                                                        *)
(*=================================================================================*)

(*	Gathers locally scoped variables - globals should be gathered independently... in fact, they
	don't really need to be gathered, but should be looked up by static address at runtime or
	by name *)
and union2 (a1, a2) (b1, b2) = (SymbolSet.union a1 b1, SymbolSet.union a2 b2)

and union2_all setlist = List.fold_left union2 (SymbolSet.empty, SymbolSet.empty) setlist

and gather_vars_expression expr =
	match expr with
	    | AspAst.Int _ | AspAst.Float _ 
	    | AspAst.String _ | AspAst.Bool _ -> SymbolSet.empty, SymbolSet.empty

	    | AspAst.BinaryOp (_, v1, v2) -> 
		      union2
			  (gather_vars_expression v1)
			  (gather_vars_expression v2)

	    | AspAst.Identifier id -> gather_vars_identifier id
	    | _ -> SymbolSet.empty, SymbolSet.empty

(* If dim is true, then this is an explicit declaration *)
and gather_vars_identifier ?(dim = false) id =
	match id with
	    | AspAst.AtomicId x ->
		      if dim then 
			      SymbolSet.empty, SymbolSet.singleton x
		      else
			      SymbolSet.singleton x, SymbolSet.empty

	    | _ -> SymbolSet.empty, SymbolSet.empty
		      

and gather_vars_statement (pos, statement) =
	match statement with
	    | AspAst.Assignment (a_id, a_val) ->
		      union2
			  (gather_vars_identifier a_id)
			  (gather_vars_expression a_val)

	    | AspAst.CompoundStatement ((AspAst.DoWhile expr), body)
	    | AspAst.CompoundStatement ((AspAst.While expr), body) ->
		      union2	(gather_vars_expression expr)
			  (union2_all (List.map gather_vars_statement body))

	    | AspAst.CompoundStatement ((AspAst.For (var, start, finish, inc)), body) ->
		      union2_all
			  [
				  SymbolSet.singleton var, SymbolSet.empty;
				  gather_vars_expression start;
				  gather_vars_expression finish;	
				  gather_vars_expression inc;	
				  union2_all (List.map gather_vars_statement body);
			  ]
	          
	    | AspAst.Dim ids ->
		      union2_all (List.map gather_vars_identifier ids)

	    (* Functions will be analyzed in their own call, so we don't recurse into them *)
	    | AspAst.Function (name, params, body) -> SymbolSet.empty, SymbolSet.empty

	    | AspAst.InlineExpr rval -> gather_vars_expression rval
	    | _ -> SymbolSet.empty, SymbolSet.empty

(* This scans for identifiers, and *then* compiles... two passes but it is
   worth it for simplicity *)
and statement_list ?(option_explicit = false) info statements = 

	(*	Gather all vars that need new allocation.  This includes all that are "dimmed" and
		all that are used without dimming UNLESS they exist already.  "dimmed" variables are
		allocated whether or not they exist globally *)
	(* TODO: replace the "gather_vars" functions with general code analysis *)
	let undeclared_firstpass, declared_vars = 
		union2_all (List.map gather_vars_statement statements)
	in

	(* We remove all the undeclared vars that are global, or all if option_explicit *)
	let undeclared_vars =
		if option_explicit then
			SymbolSet.empty
		else
			SymbolSet.diff
				undeclared_firstpass
				(SymbolMap.fold 
					 (fun key addr set -> SymbolSet.union (SymbolSet.singleton key) set)
					 info.variables
					 SymbolSet.empty)
	in
	
	(* Create allocation code for the remaining variables *)
	let newinfo, alloc_code =
		SymbolSet.fold
			(fun name (info, opcodes) ->
				 let newinfo, code = Op.alloc info name in
				 newinfo, opcodes @ code)
			(SymbolSet.union undeclared_vars declared_vars)
			(info, [])
	in
	
	(* Create the code for all the statements *)
	let final_info, statements_code = fold_statements newinfo statements in

	final_info, (alloc_code @ statements_code)

(* A non-transforming optimization *)
let rec clean_empty_writes ?(newlist = []) opcodelist =
	match opcodelist with
	    | [] -> List.rev newlist
	    | (Const (VbTypes.String "")) :: Write :: rest -> clean_empty_writes ~newlist rest
	    | op :: rest -> clean_empty_writes ~newlist:(op :: newlist) rest


let page ast = 
	let info, codelisting = statement_list empty_info ast in
	info, clean_empty_writes codelisting

