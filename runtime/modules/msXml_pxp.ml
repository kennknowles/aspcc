(** A currently empty module which should wrap either PXP or gmetadome into
	an MSXML-compatible DOM interface *)

(** possibly we could have both a PXP
	and a gmetadome module and they could compete for compatability *)

(* you'll need this, an XPath implementation on PXP:
	http://www.eleves.ens.fr/home/frisch/soft *)

open VbTools
open Run
open Pxp_yacc

(** The class of XML attributes *)
class attr = object
	
end
;;

class attr_class = object
	method create_object () = new attr
end
;;

(** The base class DOMNode *)
class dom_node = object
	inherit opaque_class

	method property ?(action = Get) name =
	match action, String.lowercase name with
	| _, "" -> raise (Symbol_not_found "Default property for 'DOMDocument'")
   
	(* Properties *)

	(*
attributes
baseName
childNodes
dataType
definition
firstChild
lastChild
namespaceURI
nextSibling
nodeName
nodeType
nodeTypedValue
nodeTypeString
nodeValue
ownerDocument
parentNode
parsed
prefix
previousSibling
specified
text
xml
*)


	(* Methods *)

(*
appendChild
cloneNode
hasChildNodes
insertBefore
removeChild
replaceChild
selectNodes
selectSingleNode
transformNode
transformNodeToObject 
*)
 
	| _, name -> raise (Symbol_not_found ("DOMNode." ^ name))


end
;;

(** The class of XMLDOM or Document or DOMDocument depending on who you ask *)
class dom_document = object
	inherit dom_node as super

	method property ?(action = Get) name =
    match action, String.lowercase name with
	| _, "" -> raise (Symbol_not_found "Default property for 'DOMDocument'")
    
    (* Properties *)
                    
    (* Methods *)
    | Get, "load" -> Builtin self#load_method
    
    | _, name -> 
		try
			super#property ~action:action name
		with
			Symbol_not_found x -> raise (Symbol_not_found ("DOMDocument." ^ x))


end
;;

class dom_document_class = object
	method create_object () = 
		let x = new dom_document in (x :> object_t)
end
;;


(** The class of DOMImplementation *)

(** Objects to be implemented:

	Attr
	CDATASection
	CharacterData
	Comment
	Document
	DocumentFragment
	DocumentType
	DOMException
	DOMImplementation
	Element
	Entity EntityReference
	HttpRequest
	NamedNodeMap
	Node
	NodeList
	Notation
	ParseError
	ProcessingInstruction
	Text
	XTLRuntime 


	This is a huge task, let's get the basic runtime working first
*)

let load runtime =
(*	add_opaque_class runtime "Attr" (new attr_class)*) ()

(*	add_opaque_class runtime "XMLDOM" (new document_class)
	add_opaque_class runtime "Microsoft.XMLDOM" (new document_class) *)

let _ =
	Loader.register_module "msXml" load
