type token =
  | HTML of (string)
  | SSI of (string * string)
  | INLINE_VALUE
  | START_DIRECTIVE
  | DIRECTIVE_NAME
  | DIRECTIVE_VALUE
  | BYREF
  | BYVAL
  | CALL
  | CASE
  | CLASS
  | CONST
  | DEFAULT
  | DIM
  | DO
  | EACH
  | END
  | ELSE
  | ELSEIF
  | ERASE
  | ERROR
  | EVAL
  | EXECUTE
  | EXIT
  | EXPLICIT
  | FOR
  | FUNCTION
  | GET
  | GOTO
  | IF
  | IN
  | INHERITS
  | IS
  | LET
  | LOOP
  | ON
  | NEXT
  | OPTION
  | PRESERVE
  | PRIVATE
  | PROPERTY
  | PUBLIC
  | RANDOMIZE
  | REDIM
  | RESUME
  | SELECT
  | SET
  | STEP
  | SUB
  | THEN
  | TO
  | UNTIL
  | WEND
  | WHILE
  | WITH
  | COMMENT of (string)
  | IDENT of (Symbol.t)
  | COLON
  | L_PAREN
  | R_PAREN
  | DOT
  | COMMA
  | EQUALS
  | NOT_EQUALS
  | LESS
  | GREATER
  | LESS_EQUAL
  | GREATER_EQUAL
  | NOT
  | AND
  | OR
  | XOR
  | IMP
  | EQV
  | PLUS
  | MINUS
  | MULT
  | DIV
  | INTDIV
  | EXP
  | MOD
  | UMINUS
  | NEW
  | CONCAT
  | INT of (int32)
  | HEX of (int32)
  | OCT of (int32)
  | FLOAT of (float)
  | STRING of (string)
  | TRUE
  | FALSE
  | NULL
  | NOTHING
  | EOF
  | EOL

val statement_list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AspAst.statement list
val rvalue :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AspAst.rvalue
val page :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AspAst.statement list
