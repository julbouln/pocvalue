(*
    pocengine - game/multimedia system
    Copyright (C) 2003-2005 POC 

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)





(** Xml object representation *)

(** {2 Types} *)

(** node binding entity type *)
type xml_entity_t=
  | TTag
  | TAttribute
  | TText
  | TNode

(** entity type *)
type xml_entity=
  | Tag of string
  | Attribute of (string*string)
  | Text of string
  | Node of (xml_entity_t,xml_entity) LinkedHashtbl.t
  | Comment of string
;;

(** {2 Exceptions} *)

exception Xml_bad_entity of string;;
exception Xml_node_binding_not_found of (xml_entity_t)
exception Xml_node_no_attrib of (string*string)
exception Xml_node_no_pcdata of string
(*exception Xml_node_no_tag of string*)

(** debug *)
let rec xml_entity_dump=function
  | Tag s->print_string("Tag: "^s);print_newline();
  | Attribute (n,s)->print_string("Attribute: "^n^"="^s);print_newline();
  | Text s->print_string("Text: "^s);print_newline();
  | _ -> ()


(** {2 Functions} *)


(** {3 Convertion functions} *)

let tag_of_entity=function
  | Tag t->t
  | _ -> raise (Xml_bad_entity "tag")

let attribute_of_entity=function
  | Attribute a->a
  | _ -> raise (Xml_bad_entity "attribute")

let text_of_entity=function
  | Text t->t
  | _ -> raise (Xml_bad_entity "text")

let node_of_entity=function
  | Node n->n
  | _ -> raise (Xml_bad_entity "node")


(** {3 Node functions} *)

(** get binding of entity type in node entity *)
let node_binding n bt=
  let ne=node_of_entity n in
    (try
       LinkedHashtbl.find ne bt
     with Not_found->
       (match bt with
	 | TTag -> Tag "NOTSET"
	 | _ ->raise (Xml_node_binding_not_found bt)))

(** get binding list of entity type in node entity *)
let node_bindings n bt=
  let ne=node_of_entity n in
    (try 
       List.rev (LinkedHashtbl.find_all ne bt)
     with Not_found->[])
       
(** convert a list of entity into a node entity *)
let node_of_list nh l=
  List.iter (
    fun e ->
      match e with
	| Tag x -> LinkedHashtbl.replace nh TTag e
	| Attribute x -> LinkedHashtbl.add nh TAttribute e
	| Text x -> LinkedHashtbl.replace nh TText e
	| Node x -> LinkedHashtbl.add nh TNode e
	| _ ->()
  ) l
    

(** xml-light interface *)
(* while im to lazy to make my own xml parser/writer *)

exception Yaxi_error 

(** convert a Xml.xml into an entity node *)
  let node_of_xml_t xt=
    let rec of_xml_t_f=function
      | Yaxi_xml.Root l->
	  of_xml_t_f (List.nth !l 0)
      | Yaxi_xml.Element (p,name,nss,attrs,children)->
	  let nh=LinkedHashtbl.create 2 in
	    (node_of_list nh
	       ([Tag (Yaxi_xml.string_of_name name)]
		@
		(List.map (
		   fun att->
		     match att with
			 Yaxi_xml.Attribute (p,name,v)->
			   Attribute (Yaxi_xml.string_of_name name,v)
		       | _ -> raise Yaxi_error
		 ) !attrs)
		@
		(List.map (
		   fun child ->
		     of_xml_t_f child
		 ) !children)
	       )
	    );
	  Node nh;

      | Yaxi_xml.Text (p,data)->Text data 
      | Yaxi_xml.Comment (p,data)-> Comment data
      | _ -> raise Yaxi_error
in
      of_xml_t_f xt



(** convert an entity into a Xml.xml *)
let node_to_xml_t nn=
  let ns=("",`None) in
  let rec to_xml_t_f=function
      | Node n ->
	  [Yaxi_xml.new_element 
	    (ns,(tag_of_entity(node_binding (Node n) TTag)))
	    ~namespaces:([(Yaxi_xml.new_namespace ns)])
	    ~attributes:(List.map (fun a->
				     let (n,v)=attribute_of_entity a in
				       Yaxi_xml.new_attribute (ns,n) v       
				  ) (node_bindings (Node n) TAttribute))
	    ~children:
	     (let tl=ref [] in
	       List.iter (
		 fun e->
		   tl:= !tl@to_xml_t_f (e)
	       ) (node_bindings (Node n) TNode);

	       (try 
		  tl:= !tl@[Yaxi_xml.new_text (text_of_entity(node_binding (Node n) TText))]
		with Xml_node_binding_not_found t->());
		 
		 !tl
	     )
	     ()
	  ]
      | _ -> []	in
      List.nth (to_xml_t_f nn) 0


(** {2 Classes} *)

(** the xml node class *)
class xml_node=
object(self)
  val mutable n=Node (LinkedHashtbl.create 2)
    
  (** Read *)

  (** get tag of this node *)
  method tag=
       tag_of_entity(node_binding n TTag)

  (** get attribute list of this node *)
  method attribs=
    List.map (fun a->attribute_of_entity a) (node_bindings n TAttribute)
      
  (** get named attribute *)
  method attrib an=
    let av=ref None in
      List.iter (
	fun a->
	  let (fan,fav)=attribute_of_entity a in
	    if fan=an then av:=Some fav	
      )
	(node_bindings n TAttribute);
      match !av with
	| Some v->v
	| None -> raise (Xml_node_no_attrib (self#tag,an))

  (** get child node list of this node *)    
  method children=
    List.map (
      fun c->
	let cn=new xml_node in
	  cn#of_node c;
	  cn
    )
      (node_bindings n TNode)

  method is_pcdata=
    (try 
       (node_binding n TText);true
     with Xml_node_binding_not_found t->false)
  (** get pcdata of this node *)
  method pcdata=
    (try 
       text_of_entity(node_binding n TText)
     with Xml_node_binding_not_found t->raise (Xml_node_no_pcdata self#tag))
  (** Write *)

  method set_tag t=
    match n with
      | Node nh->
	  LinkedHashtbl.replace nh TTag (Tag t)
      | _ -> ()

  (** add attrib (name,value) to this node *)
  method add_attrib a=
    match n with
      | Node nh->
	  LinkedHashtbl.add nh TAttribute (Attribute a)
      | _ -> ()

  (** add child to this node *)
  method add_child (c:xml_node)=
    match n with
      | Node nh->
	  LinkedHashtbl.add nh TNode (c#to_node)
      | _ -> ()

  method set_pcdata d=
    match n with
      | Node nh->
	  LinkedHashtbl.replace nh TText (Text d)
      | _ -> ()

  (** Import *)

  (** init node object from an entity node *)	  
  method of_node nn=
    (* must raise exception if nn<>Node*)
    n<-nn
  (** init node object from a list of entity *)
  method of_list l=    
      match n with
	| Node nh->(node_of_list nh l);
	| _ -> ()

  (** init node object from an Xml.xml *)	
  (* must be private *)
  method private of_xml_t xt=
    n<-node_of_xml_t xt

  (** init node object from a file 
      with automatic processing of xinclude
  *)
  method of_file f=
    let xt=Yaxi_xmlparser.parse_file f in
(*    let xt=Xml.parse_string xinc in *)
      Yaxi_xinclude.process xt;
      n<-node_of_xml_t xt.Yaxi_xml.root


  method of_string str=
    let xt=Yaxi_xmlparser.parse_string str in
      Yaxi_xinclude.process xt;
      n<-node_of_xml_t xt.Yaxi_xml.root


(** Export *)

  (** get the node entity of this node object*)	
  method to_node=
    n

  (** convert this node object to a Xml.xml *)
  method private to_xml_t=
    (node_to_xml_t n)

  (** convert this node object to string *)      
  method to_string=
    Yaxi_xml.pretty_string_of_node "" (node_to_xml_t n)


  method to_file f=
    let fo=open_out f in
      output_string fo (self#to_string); 
      close_out fo;

end;;

(** like lua_object *)
class xml_object=
object
  val mutable xml=new xml_node
  method get_xml=xml
  method set_xml x=xml<-x
  (** init xml from object *)
  method xml_to_init()=()
  (** init object from xml *)
  method xml_of_init()=()
end;;


(** wrapper to create a new node from an xml file *)
let xml_node_from_file f=
  let n=new xml_node in
    n#of_file f;
    n

