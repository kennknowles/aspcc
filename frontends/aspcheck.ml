(** Commandline documentation generator frontend to the ASPCC libraries *)

open AspParser
open Printf
module Arg = MyArg

(** {4 Command line} *)

let _ =
    let input_file = ref "" in
    Arg.parse []
	    (fun s -> input_file := s)
	    "aspcheck [no options yet] [filenames]";

    (* Process the file or stdin *)
    let lexbuf = 
        if !input_file = "" then
            Lexing.from_channel stdin
        else
            let buf = Lexing.from_channel (open_in !input_file) in
            buf.Lexing.lex_curr_p <-
            {
                Lexing.pos_fname = !input_file;
                Lexing.pos_lnum = 1;
                Lexing.pos_bol = 0;
                Lexing.pos_cnum = 0;
            };
            buf
    in
      
    let ast = AspParser.page (AspLexer.token AspLexer.Html) lexbuf in
    Typing.type_ast ast

    

