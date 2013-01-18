
(* Printing functions *)
open Printf

(*===========================================================================*)
(* For printing the Lexed stuff                                              *)
(*===========================================================================*)

open AspParser
let print_token token =
	(match token with
	     | HTML s -> printf "<html>%s</html>\n" s
	     | SSI (k,f) -> printf "<!-- #include %s=\"%s\" -->" k f
	     | INLINE_VALUE -> printf "<%%="
	     | IDENT i -> printf "%s:IDENT " (Symbol.to_string i)
	     | EOF -> printf "<EOF>\n"
	     | EOL -> printf "\n"

         | BYREF -> printf "byref "
         | BYVAL -> printf "byval "
	     | CALL -> printf "call "
         | CASE -> printf "case "
	     | CLASS -> printf "class "
	     | CONST -> printf "const "
         | DEFAULT -> printf "default "
	     | DIM -> printf "dim "
	     | DO -> printf "do "
	     | EACH -> printf "each "
	     | ELSE -> printf "else "
	     | ELSEIF -> printf "elseif "
	     | END -> printf "end "
	     | ERASE -> printf "erase "
	     | ERROR -> printf "error "
	     | EXECUTE -> printf "execute "
	     | EXIT -> printf "exit "
	     | EXPLICIT -> printf "explicit "
	     | FALSE -> printf "false "
	     | FOR -> printf "for "
	     | FUNCTION -> printf "function "
	     | GET -> printf "get "
	     | GOTO -> printf "goto "
	     | IF -> printf "if "
	     | IN -> printf "in "
	     | LOOP -> printf "loop "
	     | NEXT -> printf "next "
	     | ON -> printf "on "
	     | OPTION -> printf "option "
	     | NEW -> printf "new "
         | NOTHING -> printf "nothing "
	     | PRESERVE -> printf "preserve "
	     | PRIVATE -> printf "private "
	     | PROPERTY -> printf "property "
	     | PUBLIC -> printf "public "
	     | REDIM -> printf "redim "
	     | RESUME -> printf "resume "
         | SELECT -> printf "select "
	     | SET -> printf "set "
	     | STEP -> printf "step "
         | SUB -> printf "sub "
	     | THEN -> printf "then "
	     | TO -> printf "to "
	     | TRUE -> printf "true "
	     | WHILE -> printf "while "

	     | COMMENT c -> printf "' %s" c

	     | EQUALS -> printf "= "
         | GREATER -> printf "> "
	     | IS -> printf "is "
	     | DOT -> printf "."
	     | COMMA -> printf ","
         | COLON -> printf ":"
	     | L_PAREN -> printf "( "
	     | R_PAREN -> printf " )"

	     | PLUS -> printf " + "
	     | MINUS -> printf " - "
	     | MULT -> printf " * "
	     | DIV -> printf " / "
         | MOD -> printf "mod "
	     | INTDIV -> printf " \\ "
	     | CONCAT -> printf " & "

	     | AND -> printf " and "
	     | OR -> printf " OR "

	     | INT i -> printf "%li:INT " i
	     | FLOAT f -> printf "%f:FLOAT " f
	     | STRING s -> printf "\"%s\":STRING " s

	     | _ -> printf "*** FIXME: don't know how to print token yet ***\n");
	flush stdout


