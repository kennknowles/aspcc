%{
open Lexing
open Printf
open AspAst
exception ParseError of string

type assign_or_sub_t = Assign of rvalue | ParamList of (rvalue list)
type prop_action = [`Get | `Let | `Set]
type do_word_t = DoUntil of rvalue | DoWhile of rvalue

let error_buf = ref ""

(* TODO: restore this to its former glory of being able to accumulate errors,
	instead of just quitting - but it is moot until we get some error recovery
	rules in the grammar *)
let my_parse_error position msg =
	parse_error (sprintf "Parse error on line %i, char %i: %s" 
		position.pos_lnum 
		(position.pos_cnum - position.pos_bol)
		msg);
	raise Parsing.Parse_error

let cut_last li =
	match (List.rev li) with
	| last_item::rest -> (rest, last_item)
	| [] -> raise (Failure "Can't cut last off nothing")

let generate_while_block condition body =
    match condition with
        | DoWhile w -> CompoundStatement ((AspAst.DoWhile w), body)
        | DoUntil u -> CompoundStatement ((AspAst.DoWhile (UnaryOp (Not, u))), body)

%}
/* " This quote prevents Tuareg from mis-indenting anything */

/* html stuff */
%token <string> HTML
%token <string * string> SSI
%token INLINE_VALUE
%token START_DIRECTIVE DIRECTIVE_NAME DIRECTIVE_VALUE

/* Vb simple word tokens */
%token BYREF BYVAL CALL CASE CLASS CONST DEFAULT DIM DO 
%token EACH END ELSE ELSEIF ERASE ERROR EVAL EXECUTE EXIT EXPLICIT
%token FOR FUNCTION GET GOTO IF IN INHERITS IS
%token LET LOOP ON NEXT OPTION PRESERVE PRIVATE PROPERTY PUBLIC 
%token RANDOMIZE REDIM RESUME
%token SELECT SET STEP SUB THEN TO UNTIL 
%token WEND WHILE WITH

/* we'll reduce both rem and ' to // */
%token <string> COMMENT

%token <Symbol.t> IDENT

/* operators */
%token COLON

%token L_PAREN R_PAREN DOT COMMA
%token EQUALS NOT_EQUALS LESS GREATER LESS_EQUAL GREATER_EQUAL IS
%token NOT AND OR XOR IMP EQV
%token PLUS MINUS MULT DIV INTDIV EXP MOD
%token UMINUS NEW
%token CONCAT

/* literals */
%token <int32> INT
%token <int32> HEX
%token <int32> OCT
%token <float> FLOAT
%token <string> STRING
%token TRUE FALSE
%token NULL NOTHING

/* funny chars */
%token EOF EOL

/* precedence stuff, this is the vbscript precedence, not more normal stuff,
so don't jack it to be "better" */

/* arith have highest, then comparison, then logical */

/* These need precedence to resolve shift/reduce conflicts */
%right IF_WITHOUT_TAIL
%right COLON
%right END
%right ELSE
%right ELSEIF

/* Similarly, in the case of a single-rvalue parentheses-enclosed list,
   we always reduce the RVALUE instead of shifting it */
%right R_PAREN
%right LONE_RVALUE

/* these are non-associative in vbscript, because there is no
	 short-circuit, but for ease, i'm going to make them left */
%left IMP
%left EQV
%left XOR
%left OR
%left AND
%right NOT

%left IS
%left GREATER_EQUAL
%left LESS_EQUAL
%left GREATER
%left LESS
%left NOT_EQUALS
%left EQUALS

%left CONCAT

%left PLUS MINUS
%left MOD
%left INTDIV
%left DIV MULT
%left EXP
%right UMINUS

%left ASSIGN_EQUALS
%left DOT

%nonassoc SCRIPT_TAGS
%nonassoc OUTERMOST_SCRIPT_TAGS

%start statement_list
%type <AspAst.statement list> statement_list

%start rvalue
%type <AspAst.rvalue> rvalue

%start page							/* the entry point */
%type <AspAst.statement list> page

%%
/* 
	I view the page as html embedded in script rather than the other way
	around, primarily because of the ability to put html inside 'if'
    and 'while' statements, which violates the idea of having complete
	vbscript statments inside the html
  
*/

/* A page is either just html junk, or it is html junk surrounding a vbscript section.
   All html embedded in the main vbscript is treated differently thant the prelude and postlude */

/* The lexer should have done me the favor of eliminating all start/end script bullshit.
   The two framing HTML sections, and all embedded ones should just appear as HTML tokens,
   which are essentially just keywords of our language now */

/* ========== Collections of Statements ========== */
page:
    | statement_list { $1 }

statement_list:
    | terminated_statement statement_list { $1 @ $2 }
    | EOF   { [] }
    |       { [] }

/* This differs from statement_list in that it cannot start with
   a statement - either it has a newline or it goes into HTML SSI or INLINE */
compound_block:
    | separator statement_list   
		{ $1 @ $2 }

/* The EOF is not a 'separator' because we allow an infinite string of
   separators - and the EOF shows up that way and makes the parser loop */
terminated_statement:
    | statement EOF          { [$1] }
    | statement separator    { $1 :: $2 }
    | separator              { $1 }

at_least_one_separator:
    | separator at_least_one_separator    { $1::$2 }
    | separator                           { [$1] }

separator:
    | INLINE_VALUE rvalue   { [symbol_start_pos (), InlineExpr $2] }
    | HTML                  { [symbol_start_pos (), Html $1] }
    | SSI                   { let (kind,file) = $1 in [symbol_start_pos (), Ssi (kind, file)] }
	| COMMENT		        { [symbol_start_pos (), Comment $1] } /* guaranteed end in a newline */
	| EOL			        { [] }
	| COLON			        { [] }


/* ========== Statements ========== */

statement:
	| statement_body								{ rhs_start_pos 1, $1 }

statement_body:
	| CALL identifier								{ Call $2 }
	| CLASS permissive_ident optional_inherits compound_block END CLASS		
		    { Class ($2, $4) }
	        
	| CONST permissive_ident EQUALS rvalue			{ Const ($2, $4) }
	| DIM identifier_comma_list						{ Dim $2 }
	
	| DO do_condition compound_block LOOP
		    { generate_while_block $2 $3 }
	        
    | DO compound_block LOOP do_condition
            { generate_while_block $4 $2 }
            
	| ERASE identifier								{ Erase $2 }
	| EXECUTE rvalue                                { Execute $2 }
	| EXIT exit_keyword								{ Exit $2 }
	| FOR for_declaration compound_block NEXT	
		    { CompoundStatement ($2, $3) }
	        
	| FUNCTION subroutine_definition END FUNCTION	
		    { Function $2 }
	        
    | IF rvalue THEN if_then
        {
            let (then_block, else_block) = $4 in
            If ($2, then_block, else_block) 
        }

	| ON ERROR on_error_statement					{ $3 }
	| OPTION EXPLICIT								{ OptionExplicit }
	| member_access_level member_default member_statement
		    { MemberDef ($1, $2, $3) }
	| RANDOMIZE	L_PAREN R_PAREN        				{ Randomize }
	| RANDOMIZE	                          			{ Randomize }
	| REDIM PRESERVE identifier_comma_list { ReDim (true, $3) }
	| REDIM identifier_comma_list { ReDim (false, $2) }
			
	| select_statement								{ $1 }
	| SET identifier EQUALS rvalue					{ AspAst.Set ($2,$4) }
	| SUB subroutine_definition END SUB				{ Sub $2 }
	        
	| WHILE rvalue compound_block WEND			
		    { CompoundStatement ((While $2), $3) }
            
	| WITH identifier separator compound_block END WITH
		    { CompoundStatement ((With $2), $4) }
            
	| identifier EQUALS rvalue %prec ASSIGN_EQUALS  { Assignment ($1, $3) }

    /* These subcalls cause reduce/reduce conflicts
       ocamlyacc reduces by the first rule */
	| identifier
        { 
		    match $1 with 
            | Indices (base, rvals) -> SubCall (base, rvals)
            | _ ->	SubCall ($1, [])
        }
	| identifier rvalue_list                        { SubCall ($1, $2) }
	
	/* some helpful errors i guess */
	| FUNCTION subroutine_definition END SUB		
		{ my_parse_error (rhs_start_pos 3) "'End Sub' used with 'Function'" }

	| SUB subroutine_definition END FUNCTION
		{ my_parse_error (rhs_start_pos 3) "'End Function' used with 'Sub'" }


/* ========== Classes ========== */
optional_inherits:
	| INHERITS permissive_ident	{ Some $2 }
	|				        	{ None }

member_default:
	| DEFAULT			{ true }
	|					{ false }

member_statement:
	| identifier_comma_list 
		{	MemberIdent $1 (*match $1 with
			| AtomicId _ | Indices _ -> MemberIdent $1
			| _ -> parse_error "class member cannot have a dot"*) }

	| FUNCTION subroutine_definition END FUNCTION	{ MemberFunction $2 }
	| SUB subroutine_definition END SUB				{ MemberSub $2 }
	| PROPERTY property_action subroutine_definition END PROPERTY
		{	match $2 with
			| `Get -> PropertyGet $3
			| `Let -> PropertyLet $3
			| `Set -> PropertySet $3 }
	

member_access_level:
	| PRIVATE											{ `Private }
	| PUBLIC											{ `Public }

property_action:
	| GET 					{ `Get }
	| SET 					{ `Set }
	| LET					{ `Let }

/* ========== Do Loops ========== */
do_condition:
	| WHILE	rvalue	{ DoWhile $2 }
	| UNTIL	rvalue	{ DoUntil $2 }

/* ========== Exit Statements ========== */
exit_keyword:
	| FOR			{ `For }
	| DO			{ `Do }
	| FUNCTION		{ `Function }
	| SUB			{ `Sub }
	| PROPERTY		{ `Property }
	| WHILE			{ `While }

/* ========== For Loops ========== */
for_declaration:
	| permissive_ident EQUALS rvalue TO rvalue optional_step    { For ($1, $3, $5, $6) }
	| EACH permissive_ident IN rvalue                           { ForEach ($2, $4) }

optional_step:
	| STEP rvalue	{ $2 }
	|				{ Int (Dec, 1l) }


/* ========== If Statements ========== */
/* This construction can _always_ follow the keyword THEN */
if_then:
    | statement %prec IF_WITHOUT_TAIL   { [$1], [] }
	| statement if_else                 { [$1], $2 }
	| compound_block if_else   { $1, $2 }

/* This construction ends an if block, optionally starting it again
   via an ELSE or ELSEIF */
if_else:
	| ELSEIF rvalue THEN if_then	
        {
            let then_block, else_block = $4 in
            [rhs_start_pos 1, If ($2, then_block, else_block)]
        }
										
	| ELSE statement					        	{ [$2] }
	| ELSE statement END IF					       	{ [$2] }
    | ELSE compound_block END IF           { $2 }
    | END IF                                        { [] }

/* ========== Function Definition ========== */
subroutine_definition:
	| permissive_ident maybe_formal_param_list compound_block { $1, $2, $3 }

/* This is what may follow the function name in a FUNCTION, SUB, or PROPERTY
   definition */
maybe_formal_param_list:
	| L_PAREN formal_param_list R_PAREN	{ $2 }
	| L_PAREN R_PAREN		    		{ [] }
	|							    	{ [] }

formal_param_list:
    | formal_param                           { [$1] }
    | formal_param COMMA formal_param_list   { $1 :: $3 }

formal_param:
    | BYVAL formal_param_ident           { `ByVal, $2 }
    | BYREF formal_param_ident           { `ByRef, $2 }
    | formal_param_ident                 { `ByRef, $1 }

formal_param_ident:
    | permissive_ident      { $1 }
    /* Execute cannot be part of permissive ident because of the Execute statement...
       once it is added to the stdlib it won't even be a keyword... maybe I can
       'un-keword' it soon */
    | EXECUTE               { Symbol.of_string "Execute" }

/* ========== On Error Statements ========== */
on_error_statement:
	| RESUME NEXT				{ OnError ResumeNext }
	| GOTO INT					{	if $2 <> 0l then my_parse_error (rhs_start_pos 2) "on error goto...";
									OnError GotoZero }
	| GOTO identifier			{ OnError (Callback $2) }

/* ========== Select Statements ========== */
select_statement:
	| SELECT CASE rvalue at_least_one_separator select_statement_list END SELECT	
		{ Select ($3, $5) }

select_statement_list:
	| CASE select_rvalue compound_block select_statement_list	{ ($2, $3)::$4 }
	|											       					{ [] }

select_rvalue:
	| ELSE						{ SelectElse }
	| rvalue_list				{ SelectValue $1 }

/* ========== Expressions ========== */
rvalue:
	| L_PAREN rvalue R_PAREN		{ $2 }
	
	| identifier					{ Identifier $1 }
	| EVAL L_PAREN rvalue R_PAREN	{ Eval $3 }
	| NEW identifier				{ New $2 }

	| MINUS rvalue %prec UMINUS		{ UnaryOp (Negative, $2) }
	| rvalue PLUS rvalue			{ BinaryOp (Add, $1, $3) }
	| rvalue MINUS rvalue			{ BinaryOp (Subtract, $1, $3) }
	| rvalue MULT rvalue			{ BinaryOp (Mult, $1, $3) }
	| rvalue DIV rvalue				{ BinaryOp (Div, $1, $3) }
	| rvalue MOD rvalue				{ BinaryOp (Div, $1, $3) }
	| rvalue INTDIV rvalue			{ BinaryOp (IntDiv, $1, $3) }
	| rvalue EXP rvalue				{ BinaryOp (Exp, $1, $3) } 

	| rvalue AND rvalue		{ BinaryOp (And, $1, $3) }
	| rvalue OR rvalue		{ BinaryOp (Or, $1, $3) }
	| rvalue XOR rvalue		{ BinaryOp (Xor, $1, $3) }
	| rvalue IMP rvalue		{ BinaryOp (Imp, $1, $3) }
	| rvalue EQV rvalue		{ BinaryOp (Eqv, $1, $3) }
	| NOT rvalue			{ UnaryOp (Not, $2) }

	| rvalue EQUALS rvalue			{ BinaryOp (Equals, $1, $3) }
	| rvalue NOT_EQUALS rvalue		{ BinaryOp (NotEquals, $1, $3) }
	| rvalue LESS rvalue			{ BinaryOp (Less, $1, $3) }
	| rvalue GREATER rvalue			{ BinaryOp (Greater, $1, $3) }
	| rvalue LESS_EQUAL rvalue		{ BinaryOp (LessEqual, $1, $3) }
	| rvalue GREATER_EQUAL rvalue	{ BinaryOp (GreaterEqual, $1, $3) }
	| rvalue IS rvalue				{ BinaryOp (Is, $1, $3) }

	| rvalue CONCAT rvalue	{ BinaryOp (Concat, $1, $3) }

	| literal				{ $1 }

literal:
	| HEX					{ Int (Hex, $1) }
	| OCT					{ Int (Oct, $1) }
	| INT					{ Int (Dec, $1) }
	| FLOAT					{ Float $1 }
	| STRING				{ String $1 }
	| TRUE					{ Bool true }
	| FALSE					{ Bool false }
	| NULL					{ Null }
	| NOTHING				{ Nothing }

/*  comma separated lists of expressions */
rvalue_list:
    | rvalue %prec LONE_RVALUE  { [$1] }
	| rvalue COMMA rvalue_list  { $1 :: $3 }
    | COMMA rvalue_list         { AspAst.Null :: $2 }
	|							{ [] }

/* ========== Identifiers ========== */

identifier:
	| permissive_ident									{ AtomicId $1 }
           
 /* EXECUTE is an Irritating special case because I haven't moved it into the
            stdlib yet */

	| identifier DOT permissive_ident					{ Dot ($1, $3) }

	/* VBscript allows some really annoying identifiers */
	| identifier DOT END						{ Dot ($1, Symbol.of_string "end") }
	| identifier DOT EXECUTE						{ Dot ($1, Symbol.of_string "execute") }

	| identifier L_PAREN rvalue_list R_PAREN 
		{ Indices ($1, $3) }

identifier_comma_list:
	| identifier										{ [$1] }
	| identifier COMMA identifier_comma_list			{ $1::$3 }

/* VbScript is permissive about keywords! */
permissive_ident:
    | IDENT       { $1 }
    | PROPERTY    { Symbol.of_string "Property" }
    | ERROR       { Symbol.of_string "Error" }

/* " And this quote lets Tuareg free */
