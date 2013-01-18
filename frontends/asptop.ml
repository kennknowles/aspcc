(** Interactive toploop for testing (more useful for ASPCC implementors than users) *)

open Printf

open AspParser
module Arg = MyArg

(** {4 Command line} *)

module CommandLine = struct
    let modules = ref ["ado"; "vbPervasives"; "aspConsole"; "aspIntrinsics"; "session_hack"; "wscript"]
    let add_module x = modules := !modules @ [x]
    let inputfiles = ref []
end

let list_modules () = 
    List.iter (fun name -> Printf.printf "%s\n" name) (Runtime.module_names ());
    exit 0


open CommandLine

let argspec =
    [
	    ("-modules", Arg.String add_module, "Specify non-default module to load.");
	    
	    ("-list", Arg.Unit list_modules, "List compiled-in modules");

	    ("--", Arg.Rest (fun _ -> ()),
	     "Pass the rest of options as argv to the .asp or .aspz")

    ]

open Lexing

let parse_err_at pos =
    sprintf "Parse error in %s on line %i\n%!" 
        pos.pos_fname
        pos.pos_lnum
 
open Runtime
let load_page runtime filename =
    let chan = open_in filename in
    
	let lexbuf = Lexing.from_channel chan in
	lexbuf.lex_curr_p <- 
	{
		pos_fname = filename;
		pos_lnum = 1;
		pos_bol = 0;
		pos_cnum = 0;
	};
    
    (try
         runtime.script_path <- filename;
         let ast = AspParser.page (AspLexer.token AspLexer.Html) lexbuf in
         AstRun.page runtime ast;
         runtime.script_path <- "toploop";
     with
         | Parsing.Parse_error ->
               runtime.output (parse_err_at lexbuf.lex_curr_p));

    close_in chan


let load_toploop_functions runtime =

    List.iter (fun (name, func) -> Runtime.add_function runtime (Symbol.of_string name) func)
        [
            "show", (fun params -> 
                         ( List.iter (fun v -> 
                                          runtime.output (VbValues.debug_string !v);
                                          runtime.output "\n")
                               params;
                           
                           ref VbTypes.Null ));

            "echo", (fun params -> 
                         ( List.iter (fun v -> 
                                          runtime.output (VbValues.get_string !v);
                                          runtime.output "\n")
                               
                               params;
                           ref VbTypes.Null ));

            (* TODO: "printr" *)

            "quit", (fun params -> exit 0; ref VbTypes.Null)
        ]
    

let toploop runtime =
    let rec get_command buffer =
        match read_line () with
            | "" -> get_command buffer

            (* directives *)
            | s when s.[0] = '#'-> (
                  match Str.split (Str.regexp "[ \t]") s with
                      | ["#include"; filename] -> `Load filename
                      | ["#mode"; "eval"] -> `Switch `Eval
                      | ["#mode"; "exec"] -> `Switch `Exec
                      | _ -> `Bad
              )

            (* Normal input *)
            | str ->
                  try
                      let curr = Buffer.contents buffer in
                      let addition = String.sub str 0 (String.index str ';') in
                      Buffer.clear buffer;
                      `Go (curr ^ addition)
                  with
                      | Not_found ->
                            Buffer.add_string buffer str;
                            Buffer.add_char buffer '\n';
                            get_command buffer
    in
    
    
    let rec run_toploop runtime mode buffer =
        runtime.output "> ";
        try
            match get_command buffer with
                | `Switch `Exec -> 
                      runtime.output "[Now executing VbScript normally]\n";
                      run_toploop runtime `Exec buffer
                          
                | `Switch `Eval ->
                      runtime.output "[Now printing debug info about expressions]\n";
                      run_toploop runtime `Eval buffer
                          
                | `Load filename -> 
                      runtime.output ("[Loading '" ^ filename ^ "']\n");
                      load_page runtime filename; 
                      runtime.output "[Done]\n";
                  run_toploop runtime mode buffer
                      
                | `Bad -> runtime.output "Bad command ignored\n"; run_toploop runtime mode buffer
                | `Go str ->
                      let lexbuf = Lexing.from_string str in
	                  lexbuf.lex_curr_p <- 
	                  {
		                  pos_fname = "toploop";
		                  pos_lnum = 1;
		                  pos_bol = 0;
		                  pos_cnum = 0;
	                  };
                      
                      (try
                           match mode with
                               | `Exec -> 
                                     let ast = AspParser.statement_list 
                                                   (AspLexer.token AspLexer.VbScript) lexbuf in
                                     AstRun.page runtime ast
                                         
                               | `Eval ->
                                     let expr = AspParser.rvalue 
                                                    (AspLexer.token AspLexer.VbScript) lexbuf in
                               runtime.output (VbValues.debug_string !(AstRun.expression runtime expr));
                                     runtime.output "\n"
                       with
                       | Parsing.Parse_error ->
                             runtime.output (parse_err_at lexbuf.lex_curr_p) );
                  
                      run_toploop runtime mode buffer
        with
            | Sys.Break -> 
                  runtime.output "[Interrupted]\n";
                  run_toploop runtime mode buffer
                  
    in
    
    runtime.script_path <- "toploop";
    let b = Buffer.create 80 in
    Sys.catch_break true;
    run_toploop runtime `Exec b
   
     
let _ =
    Arg.parse argspec 
	    (fun s -> inputfiles := !inputfiles @ [s]) 
	    "asptop [options] [pre-load-files]";

    let runtime = AstRun.create_runtime () in
    Runtime.apply_all_modules runtime;
    load_toploop_functions runtime;
    
    List.iter (load_page runtime) !inputfiles;
    
    try toploop runtime
    with End_of_file -> exit 0
        


