
open Printf
open Opcode

let rec string_of_opcode ?(include_positions = true) code = 
	match code with
	| Position p ->
		if include_positions then
			sprintf "! %S %i" 
				p.Lexing.pos_fname
				p.Lexing.pos_lnum
		else
			""

	| Apply n -> sprintf "\tAPPLY %i" n
	| Atom s -> sprintf "\tATOM %S" s
	| Branch label -> sprintf "\tBRANCH %s" label
	| BranchIf label -> sprintf "\tBRANCHIF %s" label
	| BranchIfNot label -> sprintf "\tBRANCHIFNOT %s" label
	| Comment (comment, code) ->
		(match code with
		| Some c -> sprintf "%-20s;\t%s" (string_of_opcode c) comment
		| None -> sprintf "; %s" comment)
	| Const c -> (
		"\tCONST " ^
		match c with
		| VbTypes.Null -> "NULL"
		| VbTypes.Empty -> "EMPTY"
		| VbTypes.Int i -> sprintf "%li" i
		| VbTypes.Float f -> sprintf "%f" f
		| VbTypes.Bool b -> sprintf "%B" b
		| VbTypes.String s -> sprintf "%S" s
		| VbTypes.DateTime d -> sprintf "%S" "cannot yet convert datetimes to strings"
		| _ -> raise (Failure "OpcodeDump.string_of_opcode: undisplayable Const")
		)
(*	| Dim -> sprintf "\tDIM\n" *)
	| GetGlobal n -> sprintf "\tGETGLOBAL %i" n
	| GetLocal n -> sprintf "\tGETLOCAL %i" n
	| Label name -> sprintf "%s:" name
	| LetLocal n -> sprintf "\tLETLOCAL %i" n
	| LetGlobal n -> sprintf "\tLETGLOBAL %i" n
	| Push -> sprintf "\tPUSH"
	| Write -> sprintf "\tWRITE"

let dump_listing ?(include_positions = true) outchan codelist =
	List.iter 
		(fun c -> 
			let s = string_of_opcode ~include_positions c in
			if s <> "" then fprintf outchan "%s\n" s)
		codelist

