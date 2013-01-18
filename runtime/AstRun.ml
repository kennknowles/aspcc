
open AspAst
open VbValues
open VbTypes
open Printf
open Runtime
open Runtime.Scope

(* NOT THREAD SAFE, NOT GOOD, KILL... KILL!! *)
let global_html_func_hack = ref Pervasives.print_string

                                
exception UnimplementedStatement
exception Exit of AspAst.exit_target  (* what we are exiting from *)


(* i kind of wish ocaml had this, but it is trivial.. as long as it is
   custom for me, i'll return an int :-) *)

let try_select_match v vals =
	List.exists 
		(fun item -> 0 == (vb_compare v item))
		vals


let round f = Int32.of_float (f +. 0.5)
	              
let exponentiate val1 val2 =
	let result = (get_float !val1) ** (get_float !val2) in
	if (float_of_int (int_of_float result)) = result then
		Int (Int32.of_float result)
	else
		Float result

let xor b1 b2 = (b1 || b2) && not (b1 && b2)
let imp b1 b2 = (not b1) || b2
let limp i1 i2 = Int32.logor (Int32.lognot i1) i2

let leqv i1 i2 = Int32.logor 
                     (Int32.logand i1 i2)  
                     (Int32.logand 
                          (Int32.lognot i1) 
                          (Int32.lognot i2))

(* debugging only *)
let tracemsg m =
    printf "%s\n%!" m

(* let tracemsg = ignore *)
    

module Eval = struct
    (* this obeys the setting of option explicit *)
    let get_var runtime name =
	    try
		    get_variable runtime name
	    with
		    | Variable_not_found _ -> 
                  if (Runtime.get_option runtime `option_explicit) then
                      raise (Runtime.Error ("Must declare variable: " ^ (Symbol.to_string name)))
                  else
                      let value = ref Empty in
			          add_variable runtime name value;
                      value
	                  
    let rec eval_object_filter ?(return_object=false) value =
	    if return_object then value else
	        match !value with
	            | Object None -> failwith "Nothing object evaluated in non-object context."
	            | Object (Some obj) -> 
                      ( eval_object_filter ~return_object:false 
                            (!obj # m_get Symbol.empty []) )
	            | _ -> value
                      

    exception Try_next
                      
    (*==========================================================*)
    (*=================== EXPRESSION EVAL ======================*)
    (*==========================================================*)
    let rec eval runtime ?(return_object=false) (rval : AspAst.rvalue) =
        let eval = runtime.eval runtime in
	    match rval with
	        | AspAst.Identifier id -> eval_id runtime ~return_object ~apply_func:true id
	              
	        | AspAst.Eval e ->
		          let buf = Lexing.from_string (get_string !(eval e)) in
		          eval ~return_object (AspParser.rvalue (AspLexer.token AspLexer.VbScript) buf)
	        | _ ->
	              ref (
	                  match rval with
	                          
	                      (* literals and variables *)
	                      | AspAst.Int (_, i) -> Int i 
	                      | AspAst.Float f -> Float f
	                      | AspAst.String s -> String s
	                      | AspAst.Bool b -> Bool b

                          | AspAst.Nothing -> Object None

	                      | AspAst.New id -> 
		                        let the_class = (get_class runtime 
                                                     (Symbol.of_string (Vb.string_of_id id)))  in
                                !(wrap_object (the_class ()))

                          | AspAst.BinaryOp (Is, v1, v2) -> 
                                let val1, val2 =
                                    eval ~return_object:true v1,
                                    eval ~return_object:true v2
                                in
		                        (match !val1, !val2 with
		                             | Object o1, Object o2 -> Bool (o1 == o2)
		                             | _, _ -> failwith "used 'Is' with non-objects")

	                      | AspAst.BinaryOp (op, v1, v2) -> eval_binary_op runtime op v1 v2
	                      | AspAst.UnaryOp (op, v1) -> eval_unary_op runtime op v1
	                            
	                      | _ -> raise (Failure "expression not implemented yet")
	              )

                      
                      
    and get_func runtime ?(action=`Get) id : function_t =
        let dispatch_action obj property action =
            match action with
                | `Get -> obj # m_get property
                | `Let -> obj # m_let property
                | `Set -> obj # m_set property
        in
        
	    match id with
	        | AspAst.AtomicId name -> 
		          (* When faced with an AtomicId for a function:
			         1) check the current object for a member function
			         2) check the global scope for a function *)
                  
		          (*1*)
		          (try
		               (match runtime.scope.this with
		                    | Some o -> o # m_get name
		                    | _ -> raise Try_next)
		           with 
                           
                       (*2*)
                       | Member_not_found _
                       | Try_next ->
                             get_function runtime name 
                  )

	        | AspAst.Dot (base, member) ->
		          let parent = (get_object !(eval_id runtime ~return_object:true base)) in
		          (* TODO: obey private/public *)
		          dispatch_action parent member action
                      
	        | AspAst.Indices (base, indices) -> 
		          let obj = (get_object !(eval_id runtime ~return_object:true base)) in
                  dispatch_action obj Symbol.empty action
                      

    (*	If value is not an object, then this returns value immediately.
	    If value is an object, then it applies the default property to the args.
	    the return_object param could be handled by the caller, but it is
	    quite repetitive *)
    (* TODO: make sure MS does it this way.  There is some evidence that they
       only apply the "default property" rule once... sometimes.  ew *)

    (* By the way, the no-param-function-app only applies at the topmost level, i.e.
       not a base of another indexed identifier *)

    and eval_atomic_id runtime ?(return_object=false) ?(apply_func = false) name =
	    (* For atomic id's we:
	       1) try to find a variable with that name in the current scope
	       2) try to find a field with that name in the current object
	       3) try to find a function taking no args in the current scope
	       4) add it to the global scope if option_explicit not set *)

        eval_object_filter ~return_object 

	        (*1*)
	        ( try get_variable runtime name
	          with Variable_not_found _ ->
             
		          (*2*)
                  try (Runtime.get_this runtime) # field name
                  with Field_not_found _ 
                      | No_this ->
                            
		              try (Runtime.get_this runtime) # m_get name []
		              with 
                          | Member_not_found _
                          | No_this ->
                                
		                        (*3*)
		                        (* Note that this allows the Invalid_arg_list and Invalid_arg_count
		                           to trickle up, usually
		                           to the AspAst.Indices match case, I imagine *)
                                if apply_func then
                                    try (get_func runtime (AspAst.AtomicId name)) []
		                            with 
			                            | Member_not_found _
                                        | Function_not_found _ ->
                                              
		                                      (*4*)
		                                      get_var runtime name
                                else
                                    get_var runtime name)
            
    and eval_indexed_id runtime ?(return_object=false) base indices =
        let eval = (runtime.eval runtime) in
	    (* TODO: apply the func of the base to the indices first, otherwise
	       ReturnObject("x"), where ReturnObject takes no parameters,
	       might return an object/array and apply "x" to
	       its default property.  This should be an error in the arglist to
	       ReturnObject *)
	    
	    (* For indices we:
	       1) try it as a function first 
	       2) dereference the base symbol, in return_object:true context
	       3) if it is an array, then index as expected
	       4) if it is an object, then return the default dispatch
	    *)
	    let vals = List.map (eval ~return_object:true) indices in
	    try
            (*1*)
			eval_object_filter ~return_object ((get_func runtime base) vals)
        with
            | Function_not_found _
            | Member_not_found _ ->
                  
                  (*2*)
		          let lvalue = !(eval_id runtime ~return_object:true base) in
		          match lvalue with

			          (*3*)
			          | Array arr -> array_index arr (List.map (fun x -> get_int !x) vals)
                            
			          (*4*)
			          | Object (Some o) -> eval_object_filter ~return_object 
                            (!o # m_get Symbol.empty vals)
                            
			          (* we raise this exception to pass control off to function 
			             application *)
			          | _ -> Runtime.raise_error ("Not an array, object, or function")
                            
    and eval_id runtime ?(return_object=false) ?(action=`Get) ?(apply_func=false) id =
	    
	    match id with
	        | AspAst.AtomicId name ->
		          eval_atomic_id runtime ~apply_func ~return_object name

	        | AspAst.Dot (base, member) -> 
		          (*	For a dotted identifier, we evaluate the base object and then do
			            one of the following:
			            1) Try to find a field in the base object matching member
			            2) Try to find a 'property-get' (or function or sub, which are
				        equivalent to use) in the base object matching member
		          *)
	              
		          let obj = (get_object !(eval_id runtime ~apply_func:true ~return_object:true base)) in
                  
                  (*1*)
                  ( try eval_object_filter ~return_object (obj # field member)
                    with Field_not_found _ ->
                        (*2*)
		                eval_object_filter ~return_object (obj # m_get member []) )
                  
	        | AspAst.Indices (base, indices) -> 
		          eval_indexed_id runtime ~return_object base indices


    and eval_unary_op runtime op val1 =
        let eval = runtime.eval runtime in
	    match op with
	        | AspAst.Negative -> num_apply Int32.neg (~-.) (eval val1)
	        | AspAst.Not -> bool_apply (not) (Int32.lognot) (eval val1)

    and eval_binary_op runtime op val1 val2 =
        let eval = runtime.eval runtime in
        let v1, v2 = eval val1, eval val2 in
	    match op with
	        | AspAst.Concat ->
		          String ((get_string !v1) ^ (get_string !v2))
                  
	        | AspAst.Add -> num_apply2 (Int32.add) (+.) v1 v2

	        | AspAst.Subtract -> num_apply2 (Int32.sub) (-.) v1 v2
	              
	        | AspAst.Mult -> num_apply2 (Int32.mul) ( *. ) v1 v2
	              
	        | AspAst.Div -> num_apply2 (Int32.div) (/.) v1 v2

	        | AspAst.IntDiv -> 
		          Int (Int32.div 
                           (round (get_float !v1))
			               (round (get_float !v2)))
                  
	        | AspAst.Mod ->
		          Int (Int32.rem
                           (round (get_float !v1))
			               (round (get_float !v2)))

	        | AspAst.Exp -> exponentiate v1 v2

	        (* bool operators *)
            | AspAst.And -> bool_apply2 (&&) (Int32.logand) ~false_null:(Bool false) v1 v2

            | AspAst.Or -> bool_apply2 (||) (Int32.logor) ~true_null:(Bool true) v1 v2

            | AspAst.Xor -> bool_apply2 xor (Int32.logxor) v1 v2

	        (* what a bitch this null treatment is *)
            | AspAst.Imp ->
		          (match !v1, !v2 with
		               | Bool true, Null 
		               | Null, Bool false -> Null
		               | Null, Null -> Null
		               | _, _ -> bool_apply2 imp limp v1 v2)

            | AspAst.Eqv -> bool_apply2 (==) leqv v1 v2
                  
	        (* comparison operators *)

		    (* the 'vb_compare' function wraps the objects to do all the implicit
		       vbscript casts... we have to use the 'compare' instead of
		       passing the comparison function to vb_compare because the
		       type checker cannot tell the type of the function locally in
		       vb_compare *)
	        | AspAst.Equals -> Bool (0 = (vb_compare v1 v2))

		    (* <> is important here rather than != *)
            | AspAst.NotEquals -> Bool (0 <> (compare v1 v2))

            | AspAst.Less -> Bool (0 > (compare v1 v2))

            | AspAst.Greater -> Bool (0 < (compare v1 v2))

            | AspAst.LessEqual -> Bool (0 >= (compare v1 v2))

            | AspAst.GreaterEqual -> Bool (0 <= (compare v1 v2))
                  
		    (* note that the use of == is very important *)
            | AspAst.Is -> failwith "Control should never reach this point (match case handled in caller)"

end


open Eval

(*===================================================================*)
(*====================== STATEMENT EXECUTION ========================*)
(*===================================================================*)
module Exec =  struct
    let get_parsed_ssi position kind filename =
		let cwd =
			if position.Lexing.pos_fname <> "" then
				Filename.dirname position.Lexing.pos_fname
			else
				(* Even if not in CGI mode, we check the environment.  Maybe this will
				   go away eventually  *)
				try Filename.dirname (Sys.getenv "SCRIPT_FILENAME")
				with Not_found ->
					try Filename.dirname (Sys.getenv "PATH_TRANSLATED")
					with Not_found -> Sys.getcwd ()
		in
	    
		let docroot =
			try Sys.getenv "DOCUMENT_ROOT"
			with Not_found -> 
				Sys.getcwd ()
		in
		
		let includefile = match kind with
			| "virtual" -> docroot ^ "/" ^ filename
			| "file" | _ -> cwd ^ "/" ^ filename
		in
		let buf = 
			try Lexing.from_channel (open_in includefile) 
			with Sys_error _ -> 
                Runtime.raise_error (sprintf "Cannot open include file: '%s'" includefile)
		in
        
		buf.Lexing.lex_curr_p <- 
		{
			Lexing.pos_fname = includefile;	
			Lexing.pos_lnum = 1;
			Lexing.pos_bol = 0;
			Lexing.pos_cnum = 0
		};
        AspParser.page (AspLexer.token AspLexer.Html) buf
    ;;
    
    let is_return_id runtime sym =
        match runtime.scope.return with
            | Some s when s = sym -> true
            | _ -> false

    let try_let_of_this_object runtime name value =
        try
		    match runtime.scope.this with
			    | Some o -> ignore (o # m_let name [value]); true
			    | None -> false
        with
            | Member_not_found _ -> false
                  
    let try_let_field_of_this_object runtime name value =
        try
		    match runtime.scope.this with
			    | Some o -> (o # field name) := !value; true
			    | None -> false
        with
            | Field_not_found _ -> false
            

    let exec_let runtime ?(access = `Public) id value =
        let eval = runtime.eval runtime in
	    match id with
	        | AspAst.AtomicId name ->
                  
	              (*	
		                Trial and error steps: 
		                1) Assign to the return value of the current function, if it matches
                        2) Try to assign to a local variable of the current object
		                3) Execute a 'property let' on the 'this' object
		                4) Evaluate 'name' in the scope to get an object or value
                        
			            (a) If it is Nothing, error
			            (b) If it an object, invoke the default property let on value
			            (c) If it is a normal value_t ref, set it
		          *)
	              
		          (*1*)
                  if is_return_id runtime name then
		              (get_variable runtime name) := !value
                      
                  else if try_let_field_of_this_object runtime name value 
                  then
                      ()

		          (*3*)
	              else if try_let_of_this_object runtime name value then
                      ()
                  
                  (*4*)
                  else
		              let v = (eval_atomic_id runtime ~return_object:true name) in
		              (match !v with
		                   | Object None -> 
                                 Runtime.raise_error ("Attempted to invoke the \
                                                                         default Let property of \
                                                                         Nothing: "
			                                          ^ (Vb.string_of_id id))
                                 
		                   | Object (Some o) ->
			                     ignore (!o # m_let Symbol.empty [value])
	                             
		                   | _ -> v := !value)

	        | AspAst.Dot (base, member) ->
		          let parent = get_object !(eval_id runtime ~return_object:true base) in
                  ( try ignore (parent # m_let member [value])
                    with Member_not_found _ ->
                        (parent # field member) := !value )
                      
	        | AspAst.Indices (base, indices) ->
	              (*	
                        Quick rant before anyone tries to generalize this:
                        The semantics are hairy and awful, so I'm DE-generalizing some things to
                        make it easier

                        BASE ( indices )

                        BASE must be either:
                        1) An object with a default property let that accepts all the indices.
                        2) An array that accepts all the indices
                        3) A method call or function that accepts all the indices

                        Contrary to what I used to think, it will NOT apply a function of no
                        arguments in order to return an object.

                        here is what I do right now:

                        1) Check if 'base' is a function/property-let, and apply it
		                2) Check if 'base' is an array, and index into it
		                3) Check if 'base' is an object, and call the default property-let *)

		          let vals = List.map (eval ~return_object:true) indices in
		          (*1*)	
                  try ignore ((get_func runtime ~action:`Let base) (vals @ [value]))
		          with Function_not_found _ ->
                      
                      (*2*)
		              let parent = eval_id runtime ~return_object:true base in
		              match !parent with
		                      
                          (*2*)
		                  | Array arr -> 
                                let indices = List.map (fun x -> get_int !x) vals in
                                (array_index arr indices) := !value
                                
		                  | Object None -> raise (Failure
			                                          ("Attempted to invoke the default \
                                                           'Let()' property of null object: "
			                                           ^ (Vb.string_of_id id)))
                                
		                  (*3*)
		                  | Object (Some o) -> ignore (!o # m_let Symbol.empty (vals @ [value]))
		                  | _ -> failwith "Non-object, non-array used as lval in \
                                              indexed 'Let' statement."
		                                

    let exec_set runtime id value	=
        let eval = runtime.eval runtime ~return_object:true in
	    match id with
	        | AspAst.AtomicId name ->
		          let v = (eval_atomic_id runtime ~return_object:true name) in
		          (match !value with
		               | Object _ -> v := !value
		               | _ -> raise (Failure ("Tried to 'Set' non-object: " 
						                      ^ (debug_string !value))))

	        | AspAst.Dot (base, member) ->
		          let parent = get_object !(eval_id runtime ~return_object:true base) in
                  ( try ignore (parent # m_set member [value])
                    with Member_not_found _ ->
                        (parent # field member) := !value )

	        | AspAst.Indices (base, indices) ->
		          let vals = List.map eval indices in
		          let parent = eval_id runtime ~return_object:true base in
		          match !parent with
		              | Array arr -> (array_index arr
							              (List.map (fun x -> get_int !x) vals)) := !value
                            
		              | Object None -> parent := Object None
		              | Object (Some o) -> ignore (!o # m_set Symbol.empty (vals @ [value]))
		              | _ -> raise 
			                (Failure "Non-object, non-array used as lval in indexed 'Set' statement.")
    

    (* Class Stuff *)
    let exec_class_dim runtime (new_class : VbClass.user_defined_class) ?(privacy = `Public) id =
        let exec = (runtime.exec runtime) in
        let eval = (runtime.eval runtime) in
	    match id with
	        | AspAst.AtomicId name -> new_class # add_field name ~privacy None
                  
	        | AspAst.Indices (AspAst.AtomicId name, rvals) -> 
		          let indices = List.map (fun x -> get_int !(eval x)) rvals in
		          new_class # add_field name ~privacy (Some indices)

            (* TODO: replace failwith things with VbScript exceptions *)
	        | _ -> failwith "cant have a dot in a dim"
    
    let exec_class_statement runtime 
        new_class
        (pos, statement) 
        =
        let exec = (runtime.exec runtime) in
        let eval = (runtime.eval runtime) in
	    match statement with
	        | AspAst.EmptyStatement | AspAst.Comment _ -> ()
	        | AspAst.Dim id_list -> List.iter (exec_class_dim runtime new_class) id_list
                  
            | AspAst.Function (name, params, statements)
            | AspAst.Sub (name, params, statements) ->
                  new_class # add_property ~action:`Get ~privacy:`Public name params statements

	        | AspAst.MemberDef (privacy, default, id) ->
		          (match id with 
		                   (*	_ -> raise (Not_implemented "classes temporarily disabled")*)
		               | AspAst.MemberIdent mem_list -> 
			                 (if default then failwith "Can't have default field (yet)");
			                 List.iter (exec_class_dim runtime new_class ~privacy) mem_list
                                 
		               | AspAst.MemberFunction (name,params,statements) 
		               | AspAst.MemberSub (name,params,statements)
		               | AspAst.PropertyGet (name,params,statements) ->
                             if default then new_class # set_default name;
			                       new_class # add_property ~action:`Get ~privacy
                                 name params statements
		                         
		               | AspAst.PropertyLet (name,params,statements) -> ();
			                 new_class # add_property ~action:`Let ~privacy
				                 name params statements
			                     
		               | AspAst.PropertySet (name,params,statements) -> ();
			                 new_class # add_property ~action:`Set ~privacy
				                 name params statements
		          )
			      
	        | _ -> Runtime.raise_error_at pos statement "Invalid statement in class"
                  
                  
    (* The main SheBang *)
    let rec exec runtime (position, statement) =
        let exec = (runtime.exec runtime) in
        let eval = (runtime.eval runtime) in
	    try
		    match statement with
		        | AspAst.EmptyStatement | AspAst.Comment _ -> ()

		        | AspAst.Assignment (a_id, a_val) ->
			          exec_let runtime a_id (eval ~return_object:false a_val)

		        | AspAst.Call id -> 
		              (* TODO: move this into the parser *)
			          (match id with
			               | AspAst.Indices (real_id, rvals) -> 
				                 ignore ((get_func runtime real_id) (List.map eval rvals))
			               | _ -> raise (Failure "Call statement without parentheses"))
	                  
		        | AspAst.CompoundStatement (compound_type, statements) ->
			          exec_compound_statement runtime compound_type statements

		        | AspAst.Const (name, rval) -> 
			          add_variable runtime name (eval rval)
	                  
		        (* TODO: check if the var is already declared, and error
			       if option_explicit is on *)

	                  
		        | AspAst.Erase (var) -> 
			          (match !(eval_id runtime ~return_object:true var) with
			               | Array a -> () (* set each integer to zero, each string to "", each
								              object to Object None *)
			               | _ -> raise (Invalid_argument "erase"))
	                  
		        (* TODO: make statement and rval entry points for AspParser *)
		        | AspAst.Execute (rval) ->
			          let buf = Lexing.from_string (get_string !(eval rval)) in
			          (List.iter exec
				           (AspParser.statement_list (AspLexer.token AspLexer.VbScript) buf))
	                  
		        | AspAst.Exit target -> raise (Exit target)
	                  
		        | AspAst.Html html -> runtime.output html 

		        (* TODO: allow scoping of things dimmed inside an if block as an
			       optional feature *)
		        | AspAst.If (predicate, then_block, else_block) ->
			          List.iter exec (if (get_bool !(eval predicate)) 
				                      then 
                                          then_block 
                                      else 
                                          else_block)

		        | AspAst.InlineExpr e -> runtime.output (get_string !(eval e))

                (*		| AspAst.OnErrorResumeNext -> () 
			    (* runtime_options.on_error <- ResumeNext *)
		                | AspAst.OnErrorGotoZero -> ()
			    (* runtime_options.on_error <- GotoZero *)*)
	                  
		        | AspAst.OptionExplicit -> ()
			          (* runtime_options.explicit <- true *)
	                  
		        | AspAst.Randomize -> Random.self_init ()
		              
		        | AspAst.ReDim (preserve, id_list) ->
			          List.iter
				      (fun id ->
					       (match id with
					            | AspAst.Indices (id,rvals) -> 
						              let indices = List.map 
										                (fun x -> (get_int !(eval x))+1) 
										                rvals in

						              (eval_id runtime id) :=
							          (if preserve then
								           Array 
									           (merge_arrays
										            (create_array indices)
										            (get_array !(eval_id runtime id)))
							           else
								           Array (create_array indices))
					            | _ -> raise (Failure "no indices on the redim")) )
				      id_list

		        | AspAst.Select (rval, options) ->
                      List.iter exec (get_select_match runtime rval options)
			          
		        (* there is no secret assign by reference, since they are stored
			       as references *)
		        | AspAst.Set (lval, rval) -> 
			          exec_set runtime lval (eval ~return_object:true rval)

		        | AspAst.Ssi (kind, filename) ->
			          List.iter exec (get_parsed_ssi position kind filename)
                          
		        | AspAst.SubCall (id, rvals) ->
			          ignore ((get_func runtime id) (List.map (eval ~return_object:true) rvals))
                      
                (* These are all explicitly ignored, NOT unimplemented *)
                | AspAst.Dim _
                | AspAst.Class _
                | AspAst.MemberDef _
		        | AspAst.Function _
		        | AspAst.Sub _ -> ()
                      
                | _ -> raise UnimplementedStatement
	    with
            | Exit _ as exc -> raise exc
            | exc -> runtime.err runtime ~pos:position ~statement:statement
                  (match exc with
	                   | Array_out_of_bounds -> "Array index out of bounds." 
	                   | Not_object v -> (sprintf "Expected object, but got: %s" (debug_string v))	
                             
	                   | Function_not_found s -> 
                             (sprintf "Function not found: %s" (Symbol.to_string s))
                             
                       | Member_not_found (c,s) -> 
                             (sprintf "Member not found: %s.%s" 
                                  (Symbol.to_string c) (Symbol.to_string s))
                             
                       | Field_not_found (c,s) -> 
                             (sprintf "Field not found: %s.%s" 
                                  (Symbol.to_string c) (Symbol.to_string s))
                             
                       | Variable_not_found v -> (sprintf "Variable not found: %s" 
                                                      (Symbol.to_string v))
	                         
                       | Cannot_convert (v, t) -> 
                             (sprintf "Cannot convert value '%s' to %s" (debug_string v) t)
	                         
                       | Invalid_arg_list f ->
                             (sprintf "Incorrect number of arguments to %s" f)
                             
                       | Invalid_arg_count (f, wanted, got) ->
                             (sprintf "Function %s wanted %i arguments, but got %i" f wanted got)
                             
	                   | Error msg -> 
                             if Runtime.get_option runtime `use_cgi then ("<p />" ^ msg) else msg

                       | ErrorAt (pos, statement, msg) -> string_of_error pos statement 
                             (if Runtime.get_option runtime `use_cgi then ("<p />" ^ msg) else msg)
                             
		                     
                       | UnimplementedStatement -> sprintf "Unimplemented statement\n"
                             
	                   (* This is bad, it means some module didn't catch it's own errors, so we 
                          can't give a good VbScript error for it *)
	                   | exc ->
			                 (sprintf 
				                  "Some internal code was naughty and didn't catch this \
                                       Ocaml exception: %s"
				                  (Printexc.to_string exc))
                      )
                                 

    and exec_compound_statement runtime compound_type statements =
        let exec = (runtime.exec runtime) in
        let eval = (runtime.eval runtime) in
	    match compound_type with
	        | AspAst.DoWhile expr ->
		          (try
			           while (get_bool !(eval expr)) do
				           (List.iter exec statements)
			           done
		           with
			               Exit `Do -> ())

	        | AspAst.For (var, start, finish, inc) ->
		          let step, fin =	(get_int32 !(eval inc)), 
			      (get_int32 !(eval finish)) in

		          let comp = if step < 0l then (>=) else (<=) in

		          let i = ref (get_int32 !(eval start)) in
		          let var_ref = (get_var runtime var) in
		          var_ref := (Int !i);
		          (try
			           while comp !i fin do
					       var_ref := (Int !i);
					       List.iter exec statements;
					       i := Int32.add !i step
			           done
		           with
			           | Exit `For -> ())
                  
	        (* Mechanisms not in place yet, for this *)
	        | AspAst.ForEach (name, collection) ->
		          let arr = 
			          (match (get_array !(eval collection)) with
			               | Single a -> a
			               | Multi m -> raise (Failure "cannot  'for each' a mult-dim array"))
		          in
		          let var = (get_var runtime name) in
		          (try
			           Array.iter 
				           (fun item ->
					            (var := !item;
					             List.iter exec statements))
				           arr
			               
		           with
			           | Exit `For -> ())

	        | AspAst.While expr ->
		          (try
			           while (get_bool !(eval expr)) do
				           (List.iter exec statements)
			           done
		           with
			           | Exit `While -> ())

	        (* go through each member of lval, add to symbol_table, then remove
		       afterwards *)
	        | AspAst.With lval -> 
		          (try
			           ()
		           with
			           | Exit `With -> ())

    and get_select_match runtime rval options =
        let exec = (runtime.exec runtime) in
        let eval = (runtime.eval runtime) in
	    let v = eval rval in
	    match
		    List.find
			    (fun item ->
				     match item with
				         | AspAst.SelectValue vals, _ -> 
					           try_select_match v (List.map eval vals)
				         | AspAst.SelectElse, _ -> true)
			    options
	    with
		    | _, statements -> statements

    let rec exec_definition runtime (position, statement) =
        let eval = (runtime.eval runtime) in
        match statement with
		    | AspAst.Class (name, statements) ->
			      let new_class = new VbClass.user_defined_class runtime name in
			      List.iter (exec_class_statement runtime new_class) statements;
			      add_class runtime name (fun () -> new_class # create_object)
	                  
            | AspAst.MemberDef (_, _, AspAst.MemberIdent id_list)
		    | AspAst.Dim id_list ->
			      List.iter 
				  (fun id ->
					   (match id with
					        | AspAst.AtomicId name -> add_variable runtime name (ref Empty)
					        | AspAst.Indices (AspAst.AtomicId name,rvals) -> ();
						          add_variable 
                                      runtime 
                                      name 
                                      (ref (Array (create_array 
							                           (List.map (fun x -> (get_int !(eval x)) + 1)
                                                            rvals))))
					                  
					        | AspAst.Dot _ 
					        | AspAst.Indices _ -> raise (Failure "cant have a dot in a dim")) )
				  id_list
                  
            | AspAst.MemberDef (_, _, AspAst.MemberFunction (name, params, body))
            | AspAst.MemberDef (_, _, AspAst.MemberSub (name, params, body))
		    | AspAst.Function (name,params,body)
		    | AspAst.Sub (name,params,body) ->
			      add_function runtime name (runtime.apply runtime 
                                                 (create_function name params body))
		          
            | AspAst.Ssi (kind, filename) ->
			      List.iter (exec_definition runtime) (get_parsed_ssi position kind filename)

            (* I really don't feel like writing out all the things we ignore *)
            | _ -> ()
                  
    let apply_user_function 
        runtime
        (f : AspAst.statement list user_defined_function_t) 
        (args : value_t ref list) 
        : value_t ref 
        =
	    
        (* 
	       Steps in applying a user-defined function:
	       
	       1) Get the scope S that the function was defined in
	       2) Make a copy of S, S'
	       3) Add the function's name as a variable in S' to hold the return value
	       4) Add the function's name as a dispatch in S' for recursion
	       5) Add each of the function's args to S'
	       6) Run the function's body in S' 
	       7) Grab the function name out of variables in S', the return value
           
	    *)
	    
        
	    (*1,2*)
        (* We don't copy the runtime, because we want changes to runtime options, etc, to
           affect the current runtime *)
        let localscope = {(Scope.copy runtime.scope) with return = Some f.f_name} in
	    let local_runtime = {runtime with scope = localscope} in
        
	    (*3*)
	    add_variable local_runtime f.f_name (ref Empty);
        
	    (*4*)
	    add_function local_runtime f.f_name (local_runtime.apply local_runtime f);
        
	    (*5*)
	    (* TODO: optional args.. can't use iter2 in that case :( *)
        (* TODO: obey byref/byval *)
	    (try
		     List.iter2 
			     (fun (_,x) y -> add_variable local_runtime x y) 
			     f.f_args 
			     args
	     with	
		     | Invalid_argument _ -> raise (Invalid_arg_list (Symbol.to_string f.f_name)));
        
	    (*6*)
	    (try
		     List.iter (local_runtime.exec local_runtime) f.f_body;
	     with
		     | Exit `Function 
             | Exit `Sub 
             | Exit `Property -> ());
        
	    (*7*)
	    get_variable local_runtime f.f_name
			
end

let destroy_object name value =
	match !value with
	    | Object (Some o) ->	(try ignore (!o # m_get (Symbol.of_string "class_destroy") [])
							     with _ -> ())
	    | _ -> ()

open Runtime

type runtime = (AspAst.statement, AspAst.rvalue) Runtime.t

let handle_error runtime ?pos ?statement err =
    match pos, statement, Runtime.get_option runtime `use_cgi with
        | Some p, Some s, false -> Runtime.raise_error_at p s err
        | Some p, Some s, true -> Runtime.raise_error_at p s (err ^ "<p />")
        | _, _, false -> Runtime.raise_error err
        | _, _, true -> Runtime.raise_error (err ^ "<p />")

let create_runtime () =
    Runtime.create
        ~exec:Exec.exec
        ~eval:Eval.eval
        ~apply:Exec.apply_user_function
        ~err:handle_error
        ~output:print_string 

let page runtime page =
    (*let runtime = Runtime.copy runtime in*)
	try
        (* Work with a copy of the runtime, so the one passed in is not modified *)
		List.iter (Exec.exec_definition runtime) page;
		List.iter (runtime.exec runtime) page;
		Tables.SymbolTable.iter destroy_object runtime.scope.variables
	with
		| Runtime.Error msg -> runtime.output (msg ^ "\n")
		| Runtime.ErrorAt (pos,stat,msg) -> 
              runtime.output ((string_of_error pos stat msg) ^ "\n")
              (*; flush stdout; printf "%s" (Runtime.dump runtime); flush stdout*)


let statement runtime statement =
	try
        (*let runtime = Runtime.copy runtime in*)
		Exec.exec_definition runtime statement;
		runtime.exec runtime statement
	with
		| Runtime.Error msg -> runtime.output (msg ^ "\n")
		| Runtime.ErrorAt (pos,stat,msg) -> runtime.output ((string_of_error pos stat msg) ^ "\n")

let expression runtime ?(return_object=true) expr =
	try
        (*let runtime = Runtime.copy runtime in*)
		runtime.eval runtime ~return_object expr
	with
		| Runtime.Error msg -> runtime.output (msg ^ "\n"); ref Null
		| Runtime.ErrorAt (pos,stat,msg) -> runtime.output ((string_of_error pos stat msg) ^ "\n"); 
              ref Null



