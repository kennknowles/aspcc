
open VbValues
open VbTypes
open Tables

exception Error of string

exception ErrorAt of Lexing.position * AspAst.statement_body * string

(* TODO: replace Symbol_not_found with more specific exceptions? *)
exception Variable_not_found of Symbol.t
exception Function_not_found of Symbol.t
exception Class_not_found of Symbol.t
exception Member_not_found of Symbol.t * Symbol.t
exception Field_not_found of Symbol.t * Symbol.t
exception No_this

type runtime_option = 
        [
        | `on_error_resume_next
        | `option_explicit
        | `use_cgi
        ]   

module Scope = struct
    type t = {
	 	variables: (value_t ref) SymbolTable.t;
	    functions: function_t SymbolTable.t;
	    classes: class_t SymbolTable.t;  
	    this : object_t option;
	    return : Symbol.t option
    }

    let create () = {
	    classes = SymbolTable.create 50;
	    variables = SymbolTable.create 200;
	    functions = SymbolTable.create 100;
	    this = None;
	    return = None
    }
                              
    let copy scope = 
	    let newscope = {
		    classes = SymbolTable.copy scope.classes;
		    variables = SymbolTable.copy scope.variables;
		    functions = SymbolTable.copy scope.functions;
		    this = scope.this;
		    return = scope.return
	    } in
	    newscope
end


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

type error = {
	(* Description of the error *)
	err_descr : string;
	err_number : int;

	(* Where the error occured *)
	err_filename : int;
	err_linenum : int;
}

(*	all the other runtime error exceptions should be caught and turned into
	this, but this should rarely be called directly *)

(*	The options that are given values here are the definition of which may
	exist. *)
let create ~exec ~eval ~apply ~output ~err = 
	let opts = Hashtbl.create 10 in
	List.iter (fun (x,y) -> Hashtbl.add opts x y)
		[
			`on_error_resume_next, false;
			`option_explicit, false;
            `use_cgi, false
		];
	{
        exec = exec;
        eval = eval;
        apply = apply;
		output = print_string; 
        err = err;
		scope = Scope.create ();
		options = opts;
		script_path = ""
	}
;;

let copy runtime = {
	output = runtime.output;
    exec = runtime.exec;
    eval = runtime.eval;
    apply = runtime.apply;
    err = runtime.err;
	scope = Scope.copy runtime.scope;
	options = Hashtbl.copy runtime.options;
	script_path = runtime.script_path
}



let set_option runtime opt value =
	Hashtbl.replace runtime.options opt value

let get_option runtime opt =
	Hashtbl.find runtime.options opt



let add_function runtime name func =
	SymbolTable.add runtime.scope.Scope.functions name func
        
let add_class runtime name the_class =
	SymbolTable.add runtime.scope.Scope.classes name the_class

let add_variable runtime name value =
	SymbolTable.add runtime.scope.Scope.variables name value
        
let add_object runtime ~of_class name  =
	let the_class = 
        try SymbolTable.find runtime.scope.Scope.classes of_class
        with Not_found -> raise (Class_not_found of_class)
    in
    let new_obj = wrap_object (the_class ()) in
	SymbolTable.add runtime.scope.Scope.variables name new_obj


let get_variable runtime name =
    try SymbolTable.find runtime.scope.Scope.variables name
    with Not_found -> raise (Variable_not_found name)

let get_class runtime name =
    try SymbolTable.find runtime.scope.Scope.classes name
    with Not_found -> raise (Class_not_found name)

let get_function runtime name =
    try SymbolTable.find runtime.scope.Scope.functions name
    with Not_found -> raise (Function_not_found name)

let get_this runtime =
    match runtime.scope.Scope.this with
        | Some o -> o
        | None -> raise No_this


let variable_exists runtime name =
    SymbolTable.mem runtime.scope.Scope.variables name

let function_exists runtime name =
    SymbolTable.mem runtime.scope.Scope.functions name

let class_exists runtime name =
    SymbolTable.mem runtime.scope.Scope.classes name


let exec runtime statement = runtime.exec runtime statement
let eval ?(return_object = false) runtime expression = runtime.eval runtime ~return_object expression
let output runtime s = runtime.output s

open Printf
let dump runtime =
    let sorted_contents hashtbl =
        List.sort
            Pervasives.compare
            (SymbolTable.fold (fun key value li -> (Symbol.to_string key, value) :: li) hashtbl [])
    in

    sprintf "
VARIABLES:
%s
FUNCTIONS:
%s
CLASSES:
%s
"
        (List.fold_left (fun s (key, value) -> sprintf "%s%s: %S\n" s key (debug_string !value))
             ""
             (sorted_contents runtime.scope.Scope.variables))

        (List.fold_left (fun s (key, _) -> sprintf "%s%s\n" s key)
             ""
             (sorted_contents runtime.scope.Scope.functions))
        
        (List.fold_left (fun s (key, _) -> sprintf "%s%s\n" s key)
             ""
             (sorted_contents runtime.scope.Scope.classes))


open Printf
let raise_error msg =
    raise (Error msg)

let raise_error_at position statement msg =
    raise (ErrorAt (position, statement, msg))

let string_of_error position statement msg =
    sprintf "\nIn %s: Line %i: Runtime error near '%s':\n\t%s" 
		position.Lexing.pos_fname
		position.Lexing.pos_lnum 
		(Vb.string_of_statement statement) 
		msg
        
type restricted_t = (AspAst.statement, AspAst.rvalue) t

type load_function = restricted_t -> unit
    
let modules = (ref [] : (string * load_function) list ref)

let module_names () = List.map fst !modules

(* If a module is loaded twice, it will be ignored *)
let register_module name loader =
	if not (List.mem_assoc name !modules) then
		modules := !modules @ [(name, loader)]
            
let apply_module runtime name =
	(List.assoc name !modules) runtime
    
let apply_all_modules runtime =
	List.iter (fun (name,loader) -> 
                   try loader runtime
                   with exc -> raise_error (Printf.sprintf 
                                                "Error applying module '%s': %s" 
                                                name
                                                (Printexc.to_string exc))) !modules
        
(* i kind of wish ocaml had this, but it is trivial.. as long as it is
   custom for me, i'll return an int :-) *)

