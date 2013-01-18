(** A module wrapping the Gdome2 library (via GMetaDOM), supplying
	compatability with MSXML 3.0 - note that it is a work in progress. *)

(* The classes cannot directly inherit from Gdome classes because I can't 
	figure out how!  The Gdome constructors are all parameterized by a type
	that I cannot find a way to instantiate (probably because it is a
	C-defined primitive type *)

open VbTools
open VbTypes
open Gdome
open Run
open GdomeNodeTypeT

(** {4 Helper variables and functions} *)

let dom = Gdome.domImplementation ()
;;

let get_domstring x = (Gdome.domString (get_string x))

(* The annotations are to make the docs prettier *)
let domstringopt_to_string = function
	| None -> ""
	| Some (s:Gdome.domString) -> s#to_string

let nodelist_get (n:Gdome.nodeList) i : Gdome.node =
	match n#item ~index:i with
	| Some node -> node
	| None -> raise (Failure "Accessed outside of NodeList bounds")

let rec node_text node =
	let result = ref (domstringopt_to_string node#get_nodeValue) in
	let children = node#get_childNodes in
	for i = 0 to children#get_length - 1 do
		result := !result ^ (node_text (nodelist_get children i))
	done;
	!result


let not_implemented =
    Builtin (fun _ -> raise (Not_implemented "builtin function"))

(** {4 The DOM Object wrappers} *)

class dom_node_explicifier =
object
	(** Because the DOM relies on type-safety that is sort of weaker than
		ocaml, this func takes  any node and turns it into the most
		specific version.  It doesn't use the "this" node, because
		it is usually not the current node we want to change. 
		I would love to express this as a normal function, but it is
		mutually recursive with these classes, and you can't mix 'and'
		chains of different types of statements *)
	method most_specific_class (gdome_node : Gdome.node) =
		match gdome_node#get_nodeType with
		| NOT_USED_0 -> raise (Failure "not_used_0... dunno what it means")
		| ELEMENT_NODE -> (new dom_element (new element_of_node gdome_node))
		| ATTRIBUTE_NODE -> (new dom_attr (new attr_of_node gdome_node))
		| TEXT_NODE -> (new dom_text (new text_of_node gdome_node))
		| CDATA_SECTION_NODE 
			-> (new dom_cdata_section (new cdataSection_of_node gdome_node))
		| ENTITY_REFERENCE_NODE 
			-> (new dom_entity_reference 
					(new entityReference_of_node gdome_node))
		| ENTITY_NODE -> (new dom_entity (new entity_of_node gdome_node))
		| PROCESSING_INSTRUCTION_NODE
			-> (new dom_processing_instruction 
					(new processingInstruction_of_node gdome_node))
		| COMMENT_NODE -> (new dom_comment (new comment_of_node gdome_node))
		| DOCUMENT_NODE 
			-> (new dom_document (new document_of_node gdome_node))
		| DOCUMENT_TYPE_NODE 
			-> (new dom_document_type (new documentType_of_node gdome_node))
		| DOCUMENT_FRAGMENT_NODE
			-> (new dom_document_fragment 
				(new documentFragment_of_node gdome_node))
		| NOTATION_NODE
			-> (new dom_notation (new notation_of_node gdome_node))
end

(** The base class DOMNode *)
and virtual dom_node gdome_domnode = 
object(self)
    inherit opaque_object
	inherit dom_node_explicifier

	val node = (gdome_domnode : Gdome.node)

	method private node_name_get = function params ->
		arg0 "DomNode.NodeName" params;
		ref (String (node#get_nodeName#to_string))

	method private node_type = function params ->
		arg0 "DomNode.NodeType" params;
		ref (Int (
		match node#get_nodeType with
		| NOT_USED_0 -> 0
		| ELEMENT_NODE -> 1
		| ATTRIBUTE_NODE -> 2
		| TEXT_NODE -> 3
		| CDATA_SECTION_NODE -> 4
		| ENTITY_REFERENCE_NODE -> 5
		| ENTITY_NODE -> 6
		| PROCESSING_INSTRUCTION_NODE -> 7
		| COMMENT_NODE -> 8
		| DOCUMENT_NODE -> 9
		| DOCUMENT_TYPE_NODE -> 10
		| DOCUMENT_FRAGMENT_NODE -> 11
		| NOTATION_NODE -> 12
		))

	(** Microsoft extension *)
	method private node_type_string_get = function params ->
		arg0 "DomNode.NodeTypeString" params;
		let name = match node#get_nodeType with
		| NOT_USED_0 -> "not_used"
		| ELEMENT_NODE -> "element"
		| ATTRIBUTE_NODE -> "attribute"
		| TEXT_NODE -> "text"
		| CDATA_SECTION_NODE -> "cdatasection"
		| ENTITY_REFERENCE_NODE -> "entityreference"
		| ENTITY_NODE -> "entity"
		| PROCESSING_INSTRUCTION_NODE -> "processinginstruction"
		| COMMENT_NODE -> "comment"
		| DOCUMENT_NODE -> "document"
		| DOCUMENT_TYPE_NODE -> "documenttype"
		| DOCUMENT_FRAGMENT_NODE -> "documentfragment"
		| NOTATION_NODE -> "notation"
		in
		ref (String name)

	(** Text is a Microsoft extension to DOM node that consists of
		the NodeValue of this node and all its descendants concatenated
		in a pre-order traversal.
		(note that the node having a value
		is mutually exclusive with it having descendants, so it
		doesn't matter in which order it is evaluated. *)
	method private text_get = function params ->
		arg0 "DomNode.Text" params;
		ref (String (node_text node))

    method property ?(action = Get) name =
		match action, String.lowercase name with
		| _, "" -> raise (Symbol_not_found "Default property for 'DOMNode'")
		
		(* Properties *)
		
		| Get, "attributes" -> not_implemented
		| Get, "basename" -> not_implemented
		| Get, "childnodes" -> not_implemented
		| Get, "datatypes" -> not_implemented
		| Get, "definition" -> not_implemented
		| Get, "firstchild" -> not_implemented
		| Get, "lastchild" -> not_implemented
		| Get, "namespaceuri" -> not_implemented
		| Get, "nextsibling" -> not_implemented
		| Get, "nodename" -> Builtin self#node_name_get
		| Get, "nodetype" -> not_implemented
		| Get, "nodetypedvalue" -> not_implemented
		| Get, "nodetypestring" -> Builtin self#node_type_string_get
		| Get, "nodevalue" -> not_implemented
		| Get, "ownerdocument" -> not_implemented
		| Get, "parentnode" -> not_implemented
		| Get, "parsed" -> not_implemented
		| Get, "prefix" -> not_implemented
		| Get, "previoussibling" -> not_implemented
		| Get, "specified" -> not_implemented
		| Get, "text" -> Builtin self#text_get
		| Get, "xml" -> not_implemented
		
		(* Methods *)
		
		| Get, "appendchild" -> not_implemented
		| Get, "clonenode" -> not_implemented
		| Get, "haschildnodes" -> not_implemented
		| Get, "insertbefore" -> not_implemented
		| Get, "removechild" -> not_implemented
		| Get, "replacechild" -> not_implemented
		| Get, "selectnodes" -> not_implemented
		| Get, "selectsinglenode" -> not_implemented
		| Get, "transformnode" -> not_implemented
		| Get, "transformnodetoobject" -> not_implemented
		
		| _, name -> raise (Symbol_not_found ("DOMNode." ^ name))
end


(** The class of DomAttr *)
and dom_attr gdome_attr =
object(self)
	inherit dom_node (gdome_attr :> Gdome.node) as super
	val attr = (gdome_attr : Gdome.attr)

end

and dom_character_data gdome_chardata =
object(self)
	inherit dom_node (gdome_chardata :> Gdome.node) as super
	val chardata = (gdome_chardata : Gdome.characterData)

end

(** The class of DomText *)
and dom_text gdome_text =
object(self)
	inherit dom_character_data (gdome_text :> Gdome.characterData) as super

	val text = (gdome_text : Gdome.text)

end


(** The class of DomCdataSection *)
and dom_cdata_section gdome_cdata =
object(self)
	inherit dom_text (gdome_cdata :> Gdome.text)

	val cdata = (gdome_cdata : Gdome.cdataSection)
end


(** The class of DomComment *)
and dom_comment gdome_comment =
object(self)
	inherit dom_character_data (gdome_comment :> Gdome.characterData)

	val comment = (gdome_comment : Gdome.comment)
end


(** The class of DomElement *)
and dom_element gdome_element =
object (self)
	inherit dom_node (gdome_element :> Gdome.node) as super

	val element = (gdome_element : Gdome.element)

	method private get_attribute_method params =
		let attr = get_domstring !(arg1 "DomElement.GetAttribute" params) in
		ref (String (element#getAttribute attr)#to_string)

	method property ?(action = Get) name =
		match action, (String.lowercase name) with
		(* Properties *)
	
		(* Methods *)
		| Get, "getattribute" -> Builtin self#get_attribute_method
    
		| _, name ->
		try 	
			super#property ~action:action name
		with
			Symbol_not_found x -> raise (Symbol_not_found ("DomElement." ^ name))
end


(** The class of DomEntityReference *)
and dom_entity_reference gdome_entityReference =
object(self)
	inherit dom_node (gdome_entityReference :> Gdome.node) as super

	val entityReference = (gdome_entityReference : Gdome.entityReference)

end


(** The class of DomEntity *)
and dom_entity gdome_entity =
object(self)
	inherit dom_node (gdome_entity :> Gdome.node) as super

	val entity = (gdome_entity : Gdome.entity)

end


(** The class of DomProcessingInstruction *)
and dom_processing_instruction gdome_proc =
object(self)
	inherit dom_node (gdome_proc :> Gdome.node) as super

	val proc = (gdome_proc : Gdome.processingInstruction)

end


(** The class of DomDocument *)
and dom_document gdome_doc =
object(self)
	inherit dom_node (gdome_doc :> Gdome.node) as super
	val doc = (gdome_doc : Gdome.document)
	
	method private get_elements_by_tag_name_method = function params ->
		let tagname = 
			get_domstring !(arg1 "DomDocument.GetElementsByTagName" params)
		in
		let nodelist = doc#getElementsByTagName tagname in
		wrap_object ((new dom_node_list nodelist) :> object_t)

	method property ?(action = Get) name =
		match action, (String.lowercase name) with
		| Get, "getelementsbytagname" 
			-> Builtin self#get_elements_by_tag_name_method
end

(** The class of DomDocumentType *)
and dom_document_type gdome_doctype =
object(self)
	inherit dom_node (gdome_doctype :> Gdome.node) as super

	val doctype = (gdome_doctype : Gdome.documentType)

end


(** The class of DomDocumentFragment *)
and dom_document_fragment gdome_docfrag =
object(self)
	inherit dom_node (gdome_docfrag :> Gdome.node) as super

	val fragment = (gdome_docfrag : Gdome.documentFragment)

end


(** The class of DomNotation *)
and dom_notation gdome_not =
object(self)
	inherit dom_node (gdome_not :> Gdome.node) as super

	val notation = (gdome_not : Gdome.notation)

end


(** The class of node lists, including the redundant MSXML extensions *)
and dom_node_list nodelist =
object(self)
	inherit opaque_object
	inherit dom_node_explicifier

	val node_list = nodelist

	val mutable iter = 0

	method private length_get params =
		ref (Int nodelist#get_length)

	method private item_method params =
		let index = get_int !(arg1 "DomNodeList.Item" params) in
		match nodelist#item ~index:index with
		| Some node -> wrap_object ((self#most_specific_class node) :> object_t)
		| None -> ref (Object None)
	
	method private next_node_method params =
		arg0 "DomNodeList.NextNode" params;
		let item = nodelist#item ~index:iter in
		iter <- iter + 1;
		match item with
		| Some n -> wrap_object ((self#most_specific_class n) :> object_t)
		| None -> ref (Object None)

	method private reset_method params =
		arg0 "DomNodeList.Reset" params;
		iter <- 0;
		ref Null

	(** The default method returns an array of the nodes in the list
		(useful for 'for each' loops) *)
	method private default_get params =
		arg0 "DomNodeList.()" params;
		ref (Array (Single (
			Array.init 
				(nodelist#get_length)
				(fun x ->
					match nodelist#item ~index:x with
					| Some n -> wrap_object 
						((self#most_specific_class n) :> object_t)
					| None -> ref (Object None))
				)))

	method property ?(action = Get) name =
		match action, (String.lowercase name) with
		| Get, "" -> Builtin self#default_get	
	
		(* Properties *)
		| Get, "length" -> Builtin self#length_get
	
		(* Methods *)
		| Get, "item" -> Builtin self#item_method
		| Get, "nextnode" -> Builtin self#next_node_method
		| Get, "reset" -> Builtin self#reset_method
end

(** DomNamedNodeMap *)
and dom_named_node_map gdome_map =
object(self)
	inherit dom_node_explicifier
	val map = (gdome_map : Gdome.namedNodeMap)
	val mutable iter = 0

	(** {4 Properties} *)

	(** [DomNameNodeMap.Length] returns the number of nodes in this map *)
	method private length_get = function params ->
		arg0 "DomNameNodeMap.Length" params;
		ref (Int map#get_length)

	(** {4 Methods} *)

	(** [DomeNamedNodeMap.GetNamedItem(name)] returns a [DomeNode] object
		corresponding to [name] *)
	method private get_named_item_method = function params ->
		let name = get_domstring !(arg1 "DomNamedNodeMap.GetNamedItem" params)
		in
		match map#getNamedItem ~name:name with
		| Some node -> wrap_object ((self#most_specific_class node) :> object_t)
		| None -> ref (Object None)
	
	method private get_qualified_item_method = function params ->
		let name, namespace = (arg2 "DomNamedNodeMap.GetQualifiedItem" params)
		in
		match map#getNamedItemNS
				~namespaceURI:(get_domstring !namespace)
				~localName:(get_domstring !name)
		with
		| Some node -> wrap_object ((self#most_specific_class node) :> object_t)
		| None -> ref (Object None)

	method private item_method params =
		let index = get_int !(arg1 "DomNamedNodeMap.Item" params) in
		match map#item ~index:index with
		| Some node -> wrap_object ((self#most_specific_class node) :> object_t)
		| None -> ref (Object None)
	
	method private remove_named_item_method = function params ->
		let name = get_domstring 
						!(arg1 "DomNamedNodeMap.RemoveNamedItem" params)
		in
		wrap_object 
			((self#most_specific_class (map#removeNamedItem ~name:name))
				:> object_t)
	
	method private remove_qualified_item_method = function params ->
		let name, namespace = 
			(arg2 "DomNamedNodeMap.RemoveQualifiedItem" params)
		in
		wrap_object (self#most_specific_class 
			(map#removeNamedItemNS
				~namespaceURI:(get_domstring !namespace)
				~localName:(get_domstring !name)) 
			:> object_t)

	method private reset_method = function params ->
		arg0 "DomNamedNodeMap" params;
		iter <- 0;
		ref Null

	method property ?(action = Get) name =
		match action, (String.lowercase name) with
(*		| Get, "" -> Builtin self#default_get	 *)
	
		(* Properties *)
		| Get, "length" -> Builtin self#length_get
	
		(* Methods *)
		| Get, "getnameditem" -> Builtin self#get_named_item_method
		| Get, "getqualifieditem" -> Builtin self#get_qualified_item_method
		| Get, "item" -> Builtin self#item_method
		| Get, "removenameditem" -> Builtin self#remove_named_item_method
		| Get, "removequalifieditem" 
			-> Builtin self#remove_qualified_item_method
(*		| Get, "setnameditem" -> Builtin self#set_named_item_method
		| Get, "setqualifieditem" -> Bulitin self#set_qualified_item_method *)
(*		| Get, "nextnode" -> Builtin self#next_node_method *)
		| Get, "reset" -> Builtin self#reset_method
	
end
;;
(** The class of DomDocument, separated because MS has a non-DOM loading
	interface *)
class dom_xml_document doc =
object(self)
	(* Unfortunately, since MS allows a document to exist prior to actually
	being instantiated from a file, we can't inherit from dom_node because it
	requires a node to be constructed.  If we changed this, it would mean a lot
	more annoying bookkeeping *)
	inherit opaque_object
	
	(* my document lowel *)
	val mutable my_document = (doc : dom_document option)

	method private document () =
		match my_document with
		| None -> raise (Failure "No XML document loaded")
		| Some d -> d

	method private load_method = function params ->
		let xmlpath = get_string !(arg1 "DomDocument.Load" params) in
		my_document <- Some (new dom_document 
								(dom#createDocumentFromURI xmlpath ()));
		ref Null

	method property ?(action = Get) name =
    match action, String.lowercase name with
    | _, "" -> raise (Symbol_not_found "Default property for 'DOMDocument'")

    (* Properties *)

    (* Methods *)
    | Get, "load" -> Builtin self#load_method
	

	(* Pass all other properties on to the standard DomDocument class *)
    | _, name ->
        try 
			(self#document ())#property ~action:action name
        with
			Symbol_not_found x -> raise 
				(Symbol_not_found ("DomDocument." ^ name))
end
class dom_document_class = object
	method create_object () =
		let x = (new dom_xml_document None) in (x :> object_t)
end
;;




(* note that gdome is written in camel-case identifiers, and we will
	use underscores to tell the difference between them and us *)
(** The class of dom implementations, not much here *)
class dom_implementation = 
object (self)
	inherit opaque_object

	(** [domimp.HasFeature(feature, version_num)] returns true if
		the implementation has [feature] of version [version_num].  It
		appears that the valid features are ["DOM"], ["XML"], and ["MS-DOM"] *)

	(*	TODO: instead of passing this straight to gmetadom, we should indicate
		the feature that the wrapper lib has, which may include more than
		gdome2, because of our MSXML support *)
	method private has_feature_method = function params ->
		let s1, s2 = arg2 "MsXml.DomImplementation.HasFeature" params in
		ref (Bool (dom#hasFeature	~feature:(get_domstring !s1)
									~version:(get_domstring !s2)))

	method property ?(action = Get) name =
		match action, (String.lowercase name) with
		| Get, "hasfeature" -> Builtin self#has_feature_method
end
;;
(*	I would love to just have one generator class, paramaterized by what
	it is creating... TODO: figure out how :) *)
class dom_implementation_class = object
	method create_object () = new dom_implementation
end
;;



(** Objects to be implemented:
x means done
all classes at least have a skeleton, i think

	Attr
	CDATASection
	CharacterData
	Comment
	Document
	DocumentFragment
	DocumentType
	DOMException
x	DOMImplementation
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
*)

let load runtime =
	add_opaque_class runtime "DomImplementation" 
		(new dom_implementation_class);
	
	add_opaque_class runtime "Microsoft.XMLDOM" (new dom_document_class);
	add_opaque_class runtime "XMLDOM" (new dom_document_class);
	add_opaque_class runtime "DomDocument" (new dom_document_class);
		
	List.iter (fun (x, y) -> Run.add_builtin_variable runtime x (Int y))
		[
			"NODE_ELEMENT", 1;
			"NODE_ATTRIBUTE", 2;
			"NODE_TEXT", 3;
			"NODE_CDATA_SECTION", 4;
			"NODE_ENTITY_REFERENCE", 5;
			"NODE_ENTITY", 6;
			"NODE_PROCESSING_INSTRUCTION", 7;
			"NODE_COMMENT", 8;
			"NODE_DOCUMENT", 9;
			"NODE_DOCUMENT_TYPE", 10;
			"NODE_DOCUMENT_FRAGMENT", 11;
			"NODE_NOTATION", 12;
		]

let _ =
	Loader.register_module "msXml" load
