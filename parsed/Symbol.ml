(** A module for abstracting away all the annoying string lowercasing *)

type t = string

let empty = ""

let of_string s = 
    String.lowercase s

let to_string s = s

let compare = Pervasives.compare

let rec concat ?(glue = "") li =
    match li with
        | [] -> ""
        | hd :: tl -> hd ^ glue ^ (concat tl)
