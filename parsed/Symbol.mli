type t
val empty : t
val of_string : string -> t
val to_string : t -> string
val compare : t -> t -> int
val concat : ?glue:string -> t list -> t
