(** Commandline documentation generator frontend to the ASPCC libraries *)

open AspParser
open Printf
module Arg = MyArg

(** {4 Command line} *)

module CommandLine = struct
    (** Though for now I only support HTML, someone might want more later *)
    type output_format = Html
    let output_format = ref Html
    let set_output_format x () = output_format := x
	
    let input_files = ref []
		                  
    let outputfile = ref ""
end

open CommandLine

let argspec =
    [
	    ("-o", Arg.Set_string outputfile, "Specify output file.");
    ]

let is_asp filename = Filename.check_suffix filename ".asp"

let rec full_dir_contents filename =
    try
        let files = List.map 
                        (Filename.concat filename)
                        (Array.to_list (Sys.readdir filename))
        in
        List.flatten (List.map full_dir_contents files)
    with
        | Sys_error _ -> [filename]
              
let _ =
    Arg.parse argspec 
	    (fun s -> input_files := !input_files @ [s])
	    "aspdoc [-o output_dir] [filenames]";

    (* Process the file or stdin *)
    if !input_files = [] then
        exit 0
    else (
        eprintf "Initial file list: %s\n%!" (String.concat " " !input_files);
        let all_files = List.filter is_asp 
                            (List.flatten 
                                 (List.map full_dir_contents !input_files)) 
        in
        eprintf "After expansion, %i files.\n%!" (List.length all_files);
        HtmlDoc.htmldocs ?outputdir:(if "" = !outputfile then None else Some !outputfile) all_files
    )


