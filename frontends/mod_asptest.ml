(** A module to sanity check the apache interface *)

open Apache

let handler request =
	match Request.filename request with
	| None -> failwith "No filename supplied"
	| Some s ->
		try
			ignore (Str.search_forward (Str.regexp "\\.asp$") s 0);
			print_string request ("ASP file '" ^ s ^ "' intercepted by mod_asptest");
			DONE
		with
			Not_found -> DECLINED

let _ =
	Mod_caml.register_handler handler "handler"
