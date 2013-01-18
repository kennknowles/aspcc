
open VbValues
open VbTypes
open Printf
open Runtime
open Tables

let string_of_property_action = function
	| `Get -> "Get"
	| `Let -> "Let"
	| `Set -> "Set"

class user_defined_class (my_runtime : Runtime.restricted_t) my_name 
    = 
object (self)
	val mutable default = (None : Symbol.t option)
	val fields = SymbolTable.create 10
	val dispatch = PropertyTable.create 10
                       
	method name = (my_name : Symbol.t)
	method fields = fields (*: class_variable_table_t*) 
	
	method get_default = default
	method set_default name = default <- Some name

	method add_field name ?(privacy : AspAst.access_level = `Public) indices =
		SymbolTable.replace fields name (privacy, indices)
            
	method add_property 
        ?(action : property_action = `Get) 
        ?(privacy : AspAst.access_level = `Public) 
        (name : Symbol.t)
        (paramlist : (AspAst.byval * Symbol.t) list)
        (statements : AspAst.statement list) 
        =
        (* TODO: fix this hard-coded ByRef *)
        let f = 
            fun obj ->
                my_runtime.apply
                my_runtime
                {
		            f_name = name;
		            f_args = paramlist;
		            f_body = statements
	            }  
        in
        PropertyTable.add dispatch (name, action) (privacy, f)
            
    method get_property 
        ?(action : property_action = `Get) 
        ?(privacy : AspAst.access_level = `Public) 
        (name : Symbol.t)
        =
        let name = 
            match default, name = Symbol.empty with
                | _, false -> name
                | Some d, true -> d
                | None, true -> raise (Member_not_found (my_name, (Symbol.of_string "<default>")))
        in

        let priv, func = 
            try PropertyTable.find dispatch (name, action) 
            with Not_found -> raise (Member_not_found (my_name, name))
        in
        if (privacy <> `Private) && (priv = `Private) then
            raise (Member_not_found (my_name, name))
        else
            func

	method create_object =
		(new user_defined_object (self :> user_defined_class) :> object_t)
end

and user_defined_object (of_class : user_defined_class) = 
object (self)
	val my_class = of_class
	val fields = SymbolTable.create 10
                     
    method classname = my_class # name

	initializer
        SymbolTable.iter
            (fun x (priv, indices) ->
                 match indices with
                     | None -> SymbolTable.add fields x (ref Empty)
                     | Some int_list ->
                           (SymbolTable.add fields x
                                (ref (Array (create_array int_list)))))
			(my_class # fields)
	    
    method field name = 
        try SymbolTable.find fields name
        with Not_found -> raise (Field_not_found (my_class # name, name))
    
    method m_get name params = 
        (my_class # get_property ~action:`Get name ) (self :> object_t) params
        
    method m_let name params = 
        (my_class # get_property ~action:`Let name) (self :> object_t) params

    method m_set name params = 
        (my_class # get_property ~action:`Set name) (self :> object_t) params
end
;;


class virtual opaque_object =
object(self)
    method classname = Symbol.of_string (self # strname)

    method virtual strname : string

    method not_found name (params : value_t ref list) : value_t ref = 
        let s = if name = "" then "<default>" else name in
        raise (Member_not_found (self # classname, Symbol.of_string s))

    method invalid_arg_count name expected (params : value_t ref list) : value_t ref =
        let s = self # strname ^ (if name = "" then "<default>" else name) in
        raise (Invalid_arg_count (s, expected, List.length params))
                
    method field name = raise (Field_not_found (self # classname, name)); ref Null

    method m_get name = self # m_gets (Symbol.to_string name)
    method m_let name = self # m_lets (Symbol.to_string name)
    method m_set name = self # m_sets (Symbol.to_string name)

    method m_gets name = self # not_found name
    method m_lets name = self # not_found name
    method m_sets name = self # not_found name
end 
;;

class collection name arr =
object(self)
    inherit opaque_object
        
    val my_arr = arr

    method strname = name

    method m_gets name params =
        match name, params with
            | "", [] -> wrap_array my_arr

            | "", params
            | "item", params ->
                  let index = get_int !(arg1 
                                            (self # strname ^ ".Item") 
                                            params) 
                  in
                  arr.(index)
                      
            | "count", params
            | "length", params -> 
                  arg0 (self # strname ^ name) params;
                  wrap_int (Array.length arr)

            | _ -> self # not_found name params
end
;;

(* TODO
class dictionary name assoc_list =
object(self)
    inherit opaque_object
        
    val my_arr = arr

    method classname = name

    method m_get name params =
        match String.lowercase name with
            | ""
            | "item" ->
                  let index = get_int !(arg1 (self # classname ^ ".Item") params) in
                  arr.(index)
                      
            | "count"
            | "length" -> 
                  arg0 (self # classname ^ name);
                  wrap_int (Array.length arr)

            | _ -> self # not_found name params
end
*)
