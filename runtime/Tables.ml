
(** VbScript case insensitive hash tables *)

module SymbolTable = Hashtbl.Make(struct 
                                      type t = Symbol.t 
                                      let equal = (=)
                                      let hash = Hashtbl.hash
                                  end)

module PropertyTable = Hashtbl.Make(struct 
                                        type t = Symbol.t * [`Get | `Let | `Set]
                                        let equal = (=)
                                        let hash = Hashtbl.hash
                                    end)
