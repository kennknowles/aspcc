
open VbValues
open VbTypes

(*type runtime_option = [
    (** Standard options *)
    | `on_error_resume_next
    | `option_explicit
    
    (** Non-standard options *)
    | `escape_chars
] *)  

type value_t =
	| VbVal of VbTypes.value_t ref
	| Address of int
	| Empty (*	If any processing actually occurs on this, we know the VM is busted - so it is
				a good flag *)

type t = {
	(* Bookkeeping *)
	mutable html_func : string -> unit;
	mutable position : Lexing.position;
(*	options : (runtime_option, bool) Hashtbl.t *)

	(*	The actual stack machine - maybe stack isn't the
		best for vbscript, but its what I know best *)

	(* Registers *)
	mutable pc : int;
	mutable fp : int;
	mutable argc : int; (* The number of args passed to a function (so it doesn't gobble its max) *)
	mutable this : VbTypes.object_t option;

	(*	The stack.  TODO: in the future, abstract an efficient stack that utilizes "registers" *)
	mutable stack : value_t array;
	mutable stack_top : int; (* This is the top element. *)

	(* The actual code *)
	labels : (string, int) Hashtbl.t;
	code : (Opcode.t) array; (* when threaded interp, Opcode.tag should be added for branching *)

	(* The lookup table for the instructions for threaded interpretation - in the future!
	instructions : (Opcode.tag, instruction) Hashtbl.t*)
}

type instruction = t -> unit

(* Thrown whenever the type of the thing on the stack is incompatible with the
	instructions *)
exception Corrupt_stack
exception Stack_underrun

open Printf
let string_of_value value =
	match value with
	| VbVal v -> get_string !v
	| Address a -> sprintf "Addr %i" a
	| Empty -> "Empty"


let dump runtime =
	printf "
pc = %i (%s)
argc = %i
stack_top = %i
-------
%s
-------
"
		runtime.pc (OpcodeDump.string_of_opcode runtime.code.(runtime.pc))
		runtime.argc
		runtime.stack_top
		(String.concat "\n"
			(List.map 
				string_of_value
				(Array.to_list (Array.sub runtime.stack 0 (runtime.stack_top+1)))))(*;

		OpcodeDump.dump_listing stdout (Array.to_list runtime.code)*)


let push runtime value =
	runtime.stack_top <- runtime.stack_top + 1;
	try runtime.stack.(runtime.stack_top) <- value
	with Invalid_argument _ ->
		(*	If the stack needs to grow, double its size - this is a known _good_ growth solution *)
		runtime.stack <-
			Array.append 
				runtime.stack 
				(Array.make (Array.length runtime.stack) Empty);
		runtime.stack.(runtime.stack_top) <- value

let pop runtime =
	if runtime.stack_top >= 0 then
		(runtime.stack_top <- runtime.stack_top - 1;
		runtime.stack.(runtime.stack_top + 1))
	else
		raise Stack_underrun

(* These don't correspond to the opcodes, which should place these on top *)
let getlocal runtime n = runtime.stack.(runtime.fp + n)
let getglobal runtime n = runtime.stack.(n)

let setlocal runtime n value = runtime.stack.(runtime.fp + n) <- value
let setglobal runtime n value = runtime.stack.(n) <- value

(* Just for convenience I alias the stack commands *)
let pop_value runtime =
	match pop runtime with
	| VbVal v -> v
	| _ -> raise Corrupt_stack

let peek_value runtime =
	match runtime.stack.(runtime.stack_top) with
	| VbVal v -> v
	| _ -> raise Corrupt_stack

let pop_addr runtime =
	match pop runtime with
	| Address a -> a
	| _ -> raise Corrupt_stack

(* This is useful for applying a builtin function *)
let rec pop_n_values ?(li = []) runtime n =
	if n = 0 then
		List.rev li
	else
		pop_n_values ~li:((pop_value runtime) :: li) runtime (n - 1)

let make_bool v =
	VbVal (ref (VbTypes.Bool v))

open Opcode

(*	This will be needed as soon as I have objects,
	but the 'apply' function may be quite different 
let rec object_filter value =
    match !value with
    | VbTypes.Object None ->
        raise (Failure
            "Nothing object evaluated in non-object context.")

    | VbTypes.Object (Some obj) ->
        object_filter (apply (!obj#property "") [])
    | _ -> value *)

	
(* This isn't the most efficient, because we aren't operating at a level where
	it is easy to branch straight to the next instruction, like threaded code *)

let rec dispatch runtime code =
	match code with
(*		| Apply n ->
			(match pop runtime with
			(* User-defined function call *)
			| Address a -> 
				(* We push the current pc, and then an empty vbval to be the return value of the
					function *)
				push runtime (Address runtime.pc);
				push runtime (VbVal (ref VbTypes.Empty));
				runtime.argc <- n;
				runtime.pc <- (a - 1)

			(* builtin function call *)
			| VbVal v -> 
				(match !v with
				| String ">" -> 
					let val2 = pop_value runtime in
					let val1 = pop_value runtime in 
					push runtime (make_bool (0 > (compare val1 val2)))

				| String s -> 
					raise (Failure (Printf.sprintf "Cannot yet handle arbitrary function '%s'" s))
					(* apply (get_symbol runtime.builtin_functions) (pop_n_values runtime n) *) 

				| Object (Some o) ->
					raise (Failure "Cannot apply an object yet.")
					(*	(!o)#property VbTypes.Get "" *)
			
				| Object None -> 
					raise (Failure "APPLY used with nothing.")
			)
		)*)

		| Branch s -> runtime.pc <- (Hashtbl.find runtime.labels s) - 1
		| BranchIf s -> 
			(match !(pop_value runtime) with 
			| VbTypes.Bool true -> runtime.pc <- (Hashtbl.find runtime.labels s) - 1
			| _ -> ())

		| Comment (comment, None) -> ()
		| Comment (comment, Some c) -> dispatch runtime c
		| Const v -> push runtime (VbVal (ref v))
		| GetGlobal n -> push runtime (getglobal runtime n)
		| GetLocal n -> push runtime (getlocal runtime n)
		| Label s -> Hashtbl.add runtime.labels s runtime.pc
		| LetGlobal n -> setglobal runtime n (pop runtime)
		| LetLocal n -> setlocal runtime n (pop runtime)
(*		| ObjFilter -> push runtime (obj_filter (pop runtime)) *)
		| Position p -> runtime.position <- p
		| Push -> push runtime Empty
		| Write -> runtime.html_func (get_string !(pop_value runtime))


let step runtime =
	try
		dispatch runtime runtime.code.(runtime.pc);
		runtime.pc <- runtime.pc + 1
	with
		| e -> 
			Printf.printf "%s\n" (Printexc.to_string e);
			dump runtime; exit 1

let run runtime =
	let len = Array.length runtime.code in

	(* If the program counter = len then we have stepped off the EOF *)
	while runtime.pc < len do
		step runtime;
	done

let scan_for_labels codelisting =
	let table = Hashtbl.create 100 in
	let rec handle_opcode pc opcode = 
		match opcode with
		| Comment (_, Some c) -> handle_opcode pc c
		| Label s -> Hashtbl.add table s pc
		| _ -> ()
	in

	Array.iteri handle_opcode codelisting;
	table

let create codelisting = 
	{
		html_func = (fun s -> print_string s);
		position = Lexing.dummy_pos;

		pc = 1;
		fp = 0;
		argc = 0;
		this = None;
		stack = Array.make 128 Empty;
		stack_top = -1;

		code = codelisting;
		labels = scan_for_labels codelisting
	}
;;


