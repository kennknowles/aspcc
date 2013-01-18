(** Commandline frontend to the ASPCC libraries *)

open AspParser
module Arg = MyArg

(** {4 Command line} *)

module CommandLine = struct
    type action = Run | Compile | AstDump | LexDump
    let action = ref Run
    let set_action x () = action := x
			                  
    (* Compilation related *)
    let hard_ssi = ref false
    let outputfile = ref ""
    let use_stdout = ref false
    let strip = ref false

    (* Controlling runtime behavior *)
    let use_cgi = ref false
    let inputfile = ref ""
    let no_ssi = ref false
    let aspz = ref false
    let jit = ref false
		          
    let modules = ref ["ado"; "vbPervasives"; "aspConsole"; "aspIntrinsics"; "session_perl"; "wscript"]

    let add_module x = modules := !modules @ [x]

end

let list_modules () = 
    List.iter (fun name -> Printf.printf "%s\n" name) (Runtime.module_names ());
    exit 0

open CommandLine

let argspec =
    [

	    (* Compilation related *)
	    ("-c", Arg.Unit (set_action Compile), "Compile file to opcodes.");
	    
	    ("-o", Arg.Set_string outputfile, "Specify output file.");

	    ("-stdout", Arg.Set use_stdout, "Dump output to STDOUT instead of creating filename");
	    
	    ("-hardssi", Arg.Set hard_ssi,
	     "When compiling, copy in the SSI (instead of including at runtime)");
	    
	    ("-strip", Arg.Set strip,
	     "Omit the POSITION opcodes which are only for debugging.");
	    
	    (* Dumping of intermediate formats / documentation *)
	    ("-lex", Arg.Unit (set_action LexDump), 
	     "Output ugly results of lexing.");

	    ("-parse", Arg.Unit (set_action AstDump), 
	     "Output XML abstract syntax tree.");
	    
	    (* Controlling runtime behavior *)
	    ("-aspz", Arg.Set aspz,
	     "Treat input as opcodes instead of ASP (for running opcodes via stdin)");

	    ("-jit", Arg.Set jit,
	     "Compile input to opcode before running (less complete, but faster)");

	    ("-cgi", Arg.Set use_cgi,
	     "Take the file to be processed from CGI environment variables");

	    ("-nossi", Arg.Set no_ssi, "Do not emulate IIS-style server-side includes");

	    ("-modules", Arg.String add_module, "Specify non-default module to load.");
	    
	    ("-list", Arg.Unit list_modules, "List compiled-in modules");

	    ("--", Arg.Rest (fun _ -> ()),
	     "Pass the rest of options as argv to the .asp or .aspz")

    ]

open Lexing



(** {4 Actions} *)

(** Compiles and outputs the file, according to -c command line flag *)
let compile_output ?(filename = "") ast =

    (*	If we have an input file name, but no output file name, then
	    generate one *)

    let use_stdout = !CommandLine.use_stdout ||
		             match filename, !CommandLine.outputfile with
			             | "", "" -> true
			             | name, "" ->
			                   CommandLine.outputfile := (Filename.chop_extension name) ^ ".aspz";
			                   false
			             | _, name -> false
    in

    let out_chan = if use_stdout then stdout else open_out !CommandLine.outputfile in

	OpcodeDump.dump_listing 
	    out_chan 
	    ~include_positions:(not !strip) 
	    (snd (Compile.page ast));

	if not use_stdout then close_out out_chan
;;

(** Uses [LexDump] to dump some ugly lexical analysis data *)
let lex_dump lexbuf =
    let next_token = (AspLexer.token AspLexer.Html) in
	while true do
	    try
		    let token = next_token lexbuf in
		    LexDump.print_token token;
		    if token = EOF then (flush stdout; exit 0)
	    with 
		        Failure x -> AspLexer.error lexbuf.Lexing.lex_curr_p ("exception: " ^ x)
	done

(** Uses [AstDump] to dump an XML representation of the abstract syntax
  tree *)
let parse_dump ast =
    AstDump.print_page ast;
    print_string "\n";
    flush stdout

(** Runs the contents of the file; either VbScript or opcodes.  *)
let run_vbscript ?(filename = "") ast =
    let runtime = AstRun.create_runtime () in

    Runtime.set_option runtime `use_cgi !use_cgi;
    
	(* TODO: create a default module list compiled in, and then dynamically
	   add the others according to command line *)
	Runtime.apply_all_modules runtime; 
	
    runtime.Runtime.script_path <- filename;
    
	AstRun.page runtime ast;
	flush stdout

let run_opcode codelisting =
    OpcodeRun.run (OpcodeRun.create (Array.of_list codelisting))
	    

let process_channel ?filename chan =
    let use_aspz = !CommandLine.aspz ||
		           match filename with
		               | Some fname -> Filename.check_suffix fname "aspz"
		               | None -> false
    in
	
	(* If the action is "run" then we need to do further tests on
	   whether it is opcode compiled or not, so don't parse it yet. *)
	if (!CommandLine.action = Run) && use_aspz then
	    () (*run_opcode ?filename (parse_opcode chan)*)
	else
	    let lexbuf = Lexing.from_channel chan in
		lexbuf.lex_curr_p <- 
		{
		    pos_fname = (match filename with
				             | Some f -> f
				             | None -> "stdin");
		    pos_lnum = 1;
		    pos_bol = 0;
		    pos_cnum = 0;
		};

		if !action = LexDump then
		    lex_dump lexbuf
		else (
            try
		        let ast = AspParser.page (AspLexer.token AspLexer.Html) lexbuf in
			    match !action with
			        | Run ->
				          if !CommandLine.jit then
				              run_opcode (snd (Compile.page ast))
				          else
				              run_vbscript ?filename ast
                                  
			        | Compile -> compile_output ?filename ast
			        | AstDump -> parse_dump ast
			        | LexDump -> () (* control can never reach this point *)
            with
                | Parsing.Parse_error ->
                      match filename with 
                          | Some f -> Printf.eprintf "Parse error in %s on line %i\n%!" 
                                lexbuf.lex_curr_p.pos_fname
                                lexbuf.lex_curr_p.pos_lnum;
                                close_in chan; exit 1

                          | None -> Printf.eprintf "Parse error on line %i of stdin\n%!"
                                lexbuf.lex_curr_p.pos_lnum;
                                exit 1
                                    
                                    
		)

let _ =
    Arg.parse argspec 
	    (fun s -> if !inputfile = "" then inputfile := s) 
	    "aspcc [options] [filename | < filename]";
    (*	try *)

    (* If using CGI and no inputfile name given, 
       then determine filename from environment *)
    if (Filename.basename (Sys.argv.(0))) = "aspcc_cgi" then use_cgi := true;

    if !use_cgi then
	    (try inputfile := Sys.getenv "PATH_TRANSLATED"
	     with Not_found ->
	         try inputfile := Sys.getenv "SCRIPT_FILENAME"
	         with Not_found -> ());

    (* Process the file or stdin *)
    if !inputfile = "" then
	    process_channel stdin
    else
	    ( let chan = open_in !inputfile in
          process_channel ~filename:(!inputfile) chan;
	      close_in chan)
	    (*	with
	    (*	|	Parsing.Parse_error -> print_string (Asp.get_errors ()); exit 1 *)
		    |	e ->

	    (* TODO: make this less retardy *)
		    print_string "Content-type: text/plain\n\n";
		    print_string (Printexc.to_string e);
		    exit 2
	    *)


