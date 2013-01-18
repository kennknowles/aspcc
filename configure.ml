open Conf;;
open Util;;

type aspcc_module = {
    name : string;
    findlibs : string list;
    desc : string;
}

let available_modules = [
	{ name = "VbPervasives";
      findlibs = ["unix"; "netstring"];
      desc = "The documented standard VbScript functions." };
    
    { name = "aspIntrinsics";
      findlibs = ["netstring"];
      desc = "The intrinsic ASP objects minus session - currenlty uses CGI environment" };

    { name = "regexp";
      findlibs = ["pcre"];
      desc = "The RegExp object" };

    { name = "session_persil";
      findlibs = ["mysql"];
      desc = "The Session and Application ASP objects, implemented with PersiL" };

    { name = "session_perl";
      findlibs = ["perl"];
      desc = "The Session and Application ASP objects, with perl's Apache::Session" };

    { name = "session_hack";
      findlibs = ["unix"];
      desc = "The Session and Application APS objects, hacked with temp files" };

    { name = "wscript";
      findlibs = ["unix"];
      desc = "Compatability with Wscript, for accessing Argv, etc" };
    
	{ name = "scripting";
      findlibs = ["unix"]; 
      desc = "Contains only the dictionary object." };
   
    { name = "tools";
      findlibs = [];
      desc = "The MSWC Tools object." };

    { name = "browsertype";
      findlibs = [];
      desc = "The MSWC BrowserType object" };
 
	{ name = "msXml_gdome2";
      findlibs = ["gdome2"];
      desc = "A wrapper on gdome2 compatible with MSXML. (incomplete)" };
    
	{ name = "msXml_pxp";
      findlibs = ["pxp"];
      desc = "A wrapper on pxp compatible with pxp. (broken)" };
    
	{ name = "ado";
      findlibs = ["freetds"; "dbi"];
      desc = "An ADO implementation on top of ocamldbi." }
]
;;

let default_modules = [
    "VbPervasives";
    "scripting"; 
    "aspIntrinsics"; 
    "ado"; 
    "wscript";
    "tools";
    "session_hack";
    "regexp";
    "browsertype";
]
;;

let spec =
	 [
		 param "modules"
			    (StringList default_modules)
                ~doc:(
                    "\n\t\t\
                    Space-separated (must quote it!) list of modules to include - compile time only =(\n\t\t\
	    		   	Defaults to: '" ^ (String.concat " " default_modules) ^ "' \n\t\t\
		    	    Available:\n\t\t" ^
				       (String.concat "\n\t\t" 
                              (List.map 
					              (fun x -> Printf.sprintf "%s\t%s" x.name x.desc)
					                available_modules)) ^ "\n\n")

	]
;;

(* Dealing with which modules were selected *)
module StringMap = Map.Make(String)

let module_conf = Conf.configure spec

let selected_modules =
    let module_names = (module_conf # get_stringlist "modules") in
    List.filter (fun x -> List.mem x.name module_names) available_modules

let module_filename aspcc_mod =
    "runtime/modules/" ^ aspcc_mod.name ^ ".ml"

let unique li =
    let rec unique_r ?(so_far = []) li =
        match li with
            | [] -> List.rev so_far
            | entry :: rest ->
                  if List.mem entry rest then
                      unique_r ~so_far rest
                  else
                      unique_r ~so_far:(entry :: so_far) rest
    in
    unique_r li

let findlib_names =
    ["str"; "netstring"; "pcre"] 
    @ (List.flatten (List.map 
                         (fun x -> x.findlibs) 
                         selected_modules)) 

let all_findlibs =
    List.map
        (fun libname -> Conf.findlib_check libname)
        (unique findlib_names)

let findlib_conf = Conf.configure all_findlibs

open AutoMake;;

let prepend li item =
	li := item :: !li
;;

let module_sources = List.map module_filename selected_modules
;;

(* Inform the user of the configuration *)
let _ =
    print_string (findlib_conf # summarize);
    print_string (module_conf # summarize_params);
	print_endline "Now 'make' to build the utilities, and 'make install' to install them.\n"

(* The makefile generation information *)
let _ = output_makefile
        ~configuration:(module_conf :> AutoMake.configuration)

        (package
	        ~package:"aspcc"
            
	        ~version:"0.1"

	        ~sources:(split 
			              "frontends/MyArg.mli frontends/MyArg.ml
            parsed/Symbol.mli parsed/Symbol.ml
            runtime/Tables.mli runtime/Tables.ml
			parsed/AspAst.ml
			runtime/VbTypes.ml
			parsed/Opcode.ml
            runtime/VbValues.mli runtime/VbValues.ml
		    output/vb.mli output/vb.ml
			output/opcodeDump.ml
		    parsing/aspParser.mly
		    parsing/aspLexer.mll
            typing/Typing.ml
            runtime/Runtime.mli runtime/Runtime.ml
            runtime/VbClass.mli runtime/VbClass.ml
		    runtime/AstRun.mli runtime/AstRun.ml
			runtime/OpcodeRun.ml
		    output/lexDump.mli output/lexDump.ml
		    output/astDump.mli output/astDump.ml")

	        ~findlibs:findlib_names

	        ~includes:["parsed"; "parsing"; "runtime"; "output"; "frontends"; "compile"; "typing"]
	        
	        ~flags:[
                [`document], ["-I ml -colorize-code -sort -keep-code"];
		        [`mly], ["-v"];
		        [`compile; `byte], ["-g"];
	        ]

	        (* document all modules regardless of inclusion *)
	        [
		        documentation "docs" 
			     ~sources:(split 
				               "runtime/modules/vbStdLib.ml runtime/modules/scripting.ml
				runtime/modules/msXml_pxp.ml runtime/modules/msXml_gdome2.ml
				runtime/modules/aspConsole.ml
				frontends/aspcc.ml frontends/mod_aspcc.ml");
	            
		        executable "aspcc" 
			        ~sources:(module_sources @ (split "compile/compile.ml frontends/aspcc.ml"));
		       
                executable "asptop"
                    ~sources:(module_sources @ (split "compile/compile.ml frontends/asptop.ml"));
 
                executable "aspdoc" 
			        ~sources:(split "parsed/doc.mli parsed/doc.ml
                            output/HtmlDoc.mli output/HtmlDoc.ml
                            frontends/aspdoc.ml");

                executable "aspcheck"
                    ~sources:["frontends/aspcheck.ml"]
	        ]
)
;;








