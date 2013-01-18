
(** A module containing the opcodes of my little VbScript VM *)

(** The type of opcodes *)
type t =
	| Position of Lexing.position (** Tracks the position in the source file, for debugging *)
	
	| Addr of int
		(**	Just pushes an int for application - redundant with Bracnh I think *)

	| Apply of int
		(** [Apply n] Applies the function represented on the top of the stack to the next n
			stack items *)

	| Atom of string	(** Never used... *)

	| Branch of string
		(** Branch to the given label immediately *)

	| BranchIf of string
		(** If the top of the stack is true, then branch to the given label *)

	| BranchIfNot of string
		(** If the top of the stack is false, then branch to the given label *)
		(* Branch and BranchIf can emulate this, but it is uglier than bloating the opcode list :/ *)

	| Comment of string * (t option)
		(** A comment which can be on its own line or with an opcode *)

	| Const of VbTypes.value_t
		(** Pushes the given constant on to the stack *)

	| Deref of string
		(** Dereference the named field of the object on top of the stack *)

	| GetGlobal of int
		(** Gets the item at the given address, presumably a global var *)

	| GetLocal of int
		(** Gets the item at given height from the frame pointer *)

	| Label of string
		(** A label that can be branched to *)

	| LetDepth of int
		(** [LetDepth n] assigns to the item [n] deep in the stack *)

	| LetGlobal of int
		(** [Letglobal n] 
			Pops the top item of the stack and assigns its value to the item at height n from th
			bottom of the stack *)

	| LetLocal of int
		(** [LetLocal n]
			Pops the top item of the stack and assigns its value to the item at height n from th
			frame pointer *)

	| ObjFilter
		(** Repeatedly applies an objects default property get until it is a scalar value *)

	| Push 

	| Write
		(** Using whatever primitive function is registered, writes the value on top of the stack *)

type tag =
	| POSITION
	| APPLY
	| ATOM
	| CONSTBOOL
	| CONSTFLOAT
	| CONSTINT
	| CONSTSTRING
	| DEREF
	| DIM
	| LET
	| PUSH
	| WRITE
