    open AspParser
    open Lexing

    exception LexingError of string

    type lexing_mode = VbScript | Html

    let mode = ref Html
    let keyword_table = Hashtbl.create 53
    let current_string = ref ""

    (* no longer needed, this bookkeeping travels with the lexbuf in new ocamllex *)

    let next_line lexbuf =
	    lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with
		                          pos_bol = lexbuf.lex_curr_p.pos_cnum;
		                          pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1}

    let int32_of_hexstring s =
	    (Int32.of_string ( 
		     "0x" ^
		     (String.sub s 2 ((String.length s) - 2))))

    let int32_of_octstring s =
	    (Int32.of_string ( 
		     "0o" ^
		     (String.sub s 2 ((String.length s) - 2))))

    (*let enter context =
	  match context with
	  | Html -> Asp.set_context "HTML"; mode := Html
	  | VbScript -> Asp.set_context "VbScript"; mode := VbScript*)

    let error position msg =
	    raise (LexingError (Printf.sprintf "Error '%s' on line %i" msg position.pos_lnum))
            
    let _ =
	    List.iter 
		    (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
		    [   
			    "and", AND;
			    "byref", BYREF;
			    "byval", BYVAL;
			    "call", CALL;
			    "case", CASE;
			    "class", CLASS;
			    "const", CONST;
			    "default", DEFAULT;
			    "dim", DIM;
			    "do", DO;
			    "each", EACH;
			    "else", ELSE;
			    "elseif", ELSEIF;
			    "end", END;
			    "eqv", EQV;
			    "erase", ERASE;
			    "error", ERROR;
			    "eval", EVAL;
			    "execute", EXECUTE;
			    "exit", EXIT;
			    "explicit", EXPLICIT;
			    "false", FALSE;
			    "for", FOR;
			    "function", FUNCTION;
			    "get", GET;
			    "goto", GOTO;
			    "if", IF;
			    "imp", IMP;
			    "in", IN;
		   (*	    "inherits", INHERITS; *)
                "is", IS;
			    "let", LET;
			    "loop", LOOP;
			    "mod", MOD;
			    "next", NEXT;
			    "new", NEW;
			    "not", NOT;
			    "nothing", NOTHING;
			    "null", NULL;
			    "on", ON;
			    "or", OR;
			    "option", OPTION;
			    "preserve", PRESERVE;
			    "private", PRIVATE;
			    "property", PROPERTY;
			    "public", PUBLIC;
			    "randomize", RANDOMIZE;
			    "redim", REDIM;
			    "resume", RESUME;
			    "select", SELECT;
			    "true", TRUE;
			    "set", SET;
			    "step", STEP;
			    "sub", SUB;
			    "then", THEN;
			    "to", TO;
			    "until", UNTIL;
			    "wend", WEND;
			    "while", WHILE;
			    "with", WITH;
			    "xor", XOR
			]
	        
(*	enter Html *)

}

let vb_ws = [' ' '\t']

let eol = ( '\n' | '\r' | "\n\r" | "\r\n" )
    
let not_closing_script = 
    ([^ '\n' '\r' '%']* ('%' [^ '\n' '\r' '>'])* )*

rule html mode = parse
	| "#!" [^ '\r' '\n']* eol
		    {	
                (* Ignore the #! if it is there *)
                if Lexing.lexeme_start lexbuf = 0 then
				    (next_line lexbuf; ignore (lexeme lexbuf); html mode lexbuf)
			    else
				    (next_line lexbuf; HTML (lexeme lexbuf))
            }
            
	(* vbscript context *)
	| "<%" [' ' '\t']* "="			    
            {
                mode := VbScript;
                INLINE_VALUE
            }
            
	(* for now we ignore directives *)
	| "<%" [' ' '\t']* "@"			
            { ignore (asp_directive lexbuf); html mode lexbuf }
			(*		{ enter AspDirective ; START_ASP_DIRECTIVE } *)
            
	| "<%"	
            (* Instead of going straight to VB and returning something, put an
               empty HTML section in so the parser knows it was a terminator *)
			{ mode := VbScript; HTML "" }
            
	| "<!"	
			{ html_bang_rule lexbuf }
            
            
	(* match everything other than the start script tag, or ssi as HTML *)
	|	(
			(* First match all non-tag characters *)
			[^ '<' '\n' '\r']* 
		    
			(* Allow non-comment non-ASP tags *)
			('<' [^ '\r' '\n' '%' '!'])?
            
		)*
        
		(eol? as eol_option)
            
		    {
                let s = lexeme lexbuf in
                if eol_option <> "" then next_line lexbuf;
                HTML s
            }
	        
	|  eof						{ EOF }
            
(* This is, I think, the cleanest way to check for server side includes versus comments*)
and html_bang_rule = parse
	    (* Server Side Includes *)
	| "--" vb_ws* "#include" vb_ws* 
		("file" | "virtual" as kind) vb_ws* '=' vb_ws*
		'"' ([^'"']* as filename) '"' vb_ws* "-->"
        
		{ SSI (kind, filename) }
	    
	| "--"
		    { HTML "<!--" }
            
    | "DOCTYPE"
            { HTML "<!DOCTYPE" }
            
and vbrule mode = parse 
	    (* end script contex *)
	| "%>" (eol? as eol_option)
		    {
                mode := Html;
                if eol_option <> "" then next_line lexbuf;
                html mode lexbuf
            }
            
    (* Comments ending with END_SCRIPT are allowed; blast! *)
            
	| '\'' (not_closing_script as comment) (eol? as eol_option)
 	        {
                if eol_option <> "" then next_line lexbuf;
                COMMENT comment
            }
            
	(* special line continuation that should be treated like whitespace *)
	| "_" vb_ws* eol	{ next_line lexbuf; vbrule mode lexbuf }
            
	(* whitespace *)
	| vb_ws+			{ vbrule mode lexbuf }
            
	(* operators *)
	| ':'     { COLON }
            
	| '('     { L_PAREN }
	| ')'     { R_PAREN }
	        
	| '.'     { DOT }
	| ','     { COMMA }
            
	| "<>"    { NOT_EQUALS }
	| '='     { EQUALS }
	| '<'     { LESS }
	| '>'     { GREATER }
	| "<="    { LESS_EQUAL }
	| ">="    { GREATER_EQUAL }
            
	| '+'     { PLUS }
	| '-'     { MINUS }
	| '/'     { DIV }
	| '\\'    { INTDIV }
	| '*'     { MULT }
	| '^'     { EXP }
	| '&'     { CONCAT }
            
	(* literals *)
	| '&' ['h' 'H'] ['0'-'9' 'a'-'f' 'A'-'F']+
		{ HEX (int32_of_hexstring (lexeme lexbuf)) }
            
	| '&' ['o' 'O'] ['0'-'7']+
		{ OCT (int32_of_octstring (lexeme lexbuf)) }
            
	| ['0'-'9']+                  { INT (Int32.of_string (lexeme lexbuf)) }
	| ['0'-'9']+ '.' ['0'-'9']+   { FLOAT (float_of_string (lexeme lexbuf)) }
	| '"'                         { STRING (stringrule "" '"' lexbuf) }

(* I think this one maybe means it is a date, but I'll modify the syntax only when I
   can find a real reference :-) *)
	| '#'                         { STRING (stringrule "" '#' lexbuf) }
            
	        
	(* Identifiers and keywords *)
	| ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
		{  
			let id = lexeme lexbuf in
            try
				Hashtbl.find keyword_table (String.lowercase id)
			with
				| Not_found -> IDENT (Symbol.of_string id)
        }
            
	(* Funny chars - there should never be an 'eof' inside the script tags *)
	| eol				{ next_line lexbuf; EOL }
            
	(* I allow eof in vb mode because of the "execute" statement *)
	| eof		{ EOF }
            
and asp_directive = parse
	    (* end script contex *)
	| "%>" eol?				{ () }
	| not_closing_script (eol? as eol_option)
		    {
                if eol_option <> "" then next_line lexbuf;
                asp_directive lexbuf
            }
	        
            
and stringrule prefix delimiter = parse
	| "\"\"" | "##" as escaped_delim
              {
                  if delimiter = escaped_delim.[0] then
                      stringrule (prefix ^ (String.make 1 delimiter)) delimiter lexbuf
                  else
                      stringrule (prefix ^ escaped_delim) delimiter lexbuf
              }
            
	| ['"' '#'] as maybe_delim
			{
                if delimiter = maybe_delim then
                    prefix
                else
                    stringrule (prefix ^ (String.make 1 maybe_delim)) delimiter  lexbuf
            }
	| eof					{ error lexbuf.lex_curr_p "eof in string" }
	| eol					{ error lexbuf.lex_curr_p "eol in string" }
	| [^ '\r' '\n' '"' '#']+    { stringrule (prefix ^ (lexeme lexbuf)) delimiter lexbuf }
            
            { 
                (* To accomodate the multi-mode thing, here we go: *)
                (*let token mode lexbuf =
	              match !mode with
	              | Html ->			html mode lexbuf
	              | VbScript ->		vbrule mode lexbuf
                (*	| AspDirective ->	asp_directive lexbuf *)
                (*	| SsiDirective ->	ssi_directive lexbuf *)
                *)
                
                let token mode =
	                let m = ref mode in
	                (fun lexbuf ->
                         try
		                     match !m with
		                         | Html ->			html m lexbuf
		                         | VbScript ->		vbrule m lexbuf
                         with
                             | e -> 
                                   Printf.eprintf "Lexing error in %s mode %s line %i : %s"
                                   (match !m with Html -> "HTML" | VbScript -> "VbScript")
                                   lexbuf.lex_curr_p.pos_fname
                                   lexbuf.lex_curr_p.pos_lnum
                                   (Printexc.to_string e); exit 1)
                    
            }
            
