(*
    Battle For Rashitoul - The ultimate strategy/arcade game
    Copyright (C) 2003,2004,2005 POC 

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

open Unix;;

open Xml;;
open XmlParser;;

open Value_xinclude;;

(** Xml parse in an object way *)


(** types *)

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
  | Node of (xml_entity_t,xml_entity) LinkedHashtbl.t;;

(** exceptions *)

exception Xml_bad_entity of string;;
exception Xml_node_binding_not_found of xml_entity_t
exception Xml_node_no_attrib of string


(** debug *)
let rec xml_entity_dump=function
  | Tag s->print_string("Tag: "^s);print_newline();
  | Attribute (n,s)->print_string("Attribute: "^n^"="^s);print_newline();
  | Text s->print_string("Text: "^s);print_newline();
  | _ -> ()


(** conversion funcs *)

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


(** node funcs *)

(** get binding of entity type in node entity *)
let node_binding n bt=
  let ne=node_of_entity n in
    (try
       LinkedHashtbl.find ne bt
     with Not_found->raise (Xml_node_binding_not_found bt))

(** get binding list of entity type in node entity *)
let node_bindings n bt=
  let ne=node_of_entity n in
    (try 
       List.rev (LinkedHashtbl.find_all ne bt)
     with Not_found->[])
       
(** convert a list of entity into a node entity *)
let node_of_list l=
  let nh=LinkedHashtbl.create 2 in
    List.iter (
      fun e ->
	match e with
	  | Tag x -> LinkedHashtbl.add nh TTag e
	  | Attribute x -> LinkedHashtbl.add nh TAttribute e
	  | Text x -> LinkedHashtbl.add nh TText e
	  | Node x -> LinkedHashtbl.add nh TNode e
    ) l;
    nh

(** xml-light interface *)
(* while im to lazy to make my own xml parser/writer *)

(** convert a Xml.xml into an entity node *)
  let node_of_xml_t xt=
    let rec of_xml_t_f=function
      | Element (tag,attrs,children)->
	  Node 
	    (node_of_list
	       ([Tag tag]
		@
		(List.map (
		   fun att->
		     Attribute att
		 ) attrs)
		@
		(List.map (
		   fun child ->
		     of_xml_t_f child
		 ) children)
	       )
	    )
      | PCData data->Text data in
      of_xml_t_f xt

(** convert an entity into a Xml.xml *)
  let node_to_xml_t nn=
    let rec to_xml_t_f=function
      | Text t->[PCData t]
      | Node n ->
	  [Element (tag_of_entity(node_binding (Node n) TTag),
		    List.map (fun a->attribute_of_entity a) (node_bindings (Node n) TAttribute),
		    let tl=ref [] in
		      List.iter (
			fun e->
			  tl:= !tl@to_xml_t_f (e)
		      ) (node_bindings (Node n) TNode);
		      !tl
		   )
	  ]
      | _ -> []	in
      List.nth (to_xml_t_f nn) 0




(** the xml node class *)
class xml_node=
object(self)
  val mutable n=Node (LinkedHashtbl.create 2)
    
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
	| None -> raise (Xml_node_no_attrib an)

  (** get child node list of this node *)    
  method children=
    List.map (
      fun c->
	let cn=new xml_node in
	  cn#of_node c;
	  cn
    )
      (node_bindings n TNode)

  (** get pcdata of this node *)
  method pcdata=
  (* FIXME must raise a Xml_node_no_pcdata exception if no pcdata *)
    text_of_entity(node_binding n TText)

  (** modification *)

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

  (** import *)

  (** init node object from an entity node *)	  
  method of_node nn=
    (* must raise exception if nn<>Node*)
    n<-nn
  (** init node object from a list of entity *)
  method of_list l=
    n<-Node (node_of_list l)

  (** init node object from an Xml.xml *)	
  (* must be private *)
  method of_xml_t xt=
    n<-node_of_xml_t xt

  (** init node object from a file 
      with automatic processing of xinclude
  *)
  method of_file f=
    let xinc=xinclude_process_file f in
    let xt=Xml.parse_string xinc in
      n<-node_of_xml_t xt


(** export *)

  (** get the node entity of this node object*)	
  method to_node=
    n

  (** convert this node object to a Xml.xml *)
  method to_xml_t=
    (node_to_xml_t n)

  (** convert this node object to string *)      
  method to_string=
    Xml.to_string (node_to_xml_t n)

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


(* FIXME must go in value_xmlparser.ml *)
(* PARSER *)

class virtual xml_parser=
object(self)
  method tag=""
  method virtual parse_attr: string->string->unit
  method virtual parse_child: string->xml_node->unit

  method parse (n:xml_node)= 
    List.iter (
      fun (an,av)->
      self#parse_attr an av
    ) n#attribs;

    List.iter (
      fun (cn)->
      self#parse_child cn#tag cn
    ) n#children;

end;;





(* XML : General parsers *)



(* XML : int parser of form <tag a="int"> *)
class xml_int_parser a=
object
  inherit xml_parser

  val mutable n=0
(*  method get_int=n *)
  method get_val=n

  method parse_attr k v=
    match k with
      | r when r=a -> n<-int_of_string v
      | _ ->()
  method parse_child k v=()


end;;

(* XML : string parser of form <tag a="string"> *)
class xml_string_parser a=
object
  inherit xml_parser

  val mutable n=""
  method get_val=n

  method parse_attr k v=
    match k with
      | r when r=a -> n<-v
      | _ ->()
  method parse_child k v=()


end;;


(* XML : point parser of form <tag x="int" y="int"> *)
class xml_point_parser=
object
  inherit xml_parser

  val mutable x=0
  val mutable y=0
  method get_x=x
  method get_y=y

  method parse_attr k v=
    match k with
      | "x" -> x<-int_of_string v
      | "y" -> y<-int_of_string v
      | _ -> ()
  method parse_child k v=()


end;;

(* XML : size parser of form <tag w="int" h="int"> *)
class xml_size_parser=
object
  inherit xml_parser

  val mutable w=0
  val mutable h=0
  method get_w=w
  method get_h=h

  method parse_attr k v=
    match k with
      | "w" -> w<-int_of_string v
      | "h" -> h<-int_of_string v
      | _ -> ()
  method parse_child k v=()


end;;


(* XML : size parser of form <tag r="int" g="int" b="int"> *)
class xml_color_parser=
object
  inherit xml_parser

  val mutable r=0
  val mutable g=0
  val mutable b=0

  method get_val=(r,g,b)
  method get_color=(r,g,b)
  method get_r=r
  method get_g=g
  method get_b=b

  method parse_attr k v=
(*    print_string "add color";print_newline(); *)
    match k with
      | "r" -> r<-int_of_string v
      | "g" -> g<-int_of_string v
      | "b" -> b<-int_of_string v
      | _ -> ()
  method parse_child k v=()


end;;

(* XML : list parser  *)
class ['a,'b] xml_list_parser ct (pc:unit->'b) =
object
  inherit xml_parser
  val mutable parser_func=pc
  method set_parser_func (npc:unit->'b)=parser_func<-npc

  val mutable frms=DynArray.create()
  method get_list=(DynArray.to_list frms : 'a list)
  method get_array=(DynArray.to_array frms : 'a array)
(*  method get_val n=(DynArray.get frms n : 'a) *)

  method parse_attr k v=()
  method parse_child k v=
    match k with
      | r when r=ct -> let p=parser_func() in p#parse v;DynArray.add frms p#get_val
      | _ -> ()
	      
end;;

class xml_intlist_parser ct pc=
object
  inherit [int,xml_int_parser] xml_list_parser ct pc
end;;

class xml_stringlist_parser ct pc=
object
  inherit [string,xml_string_parser] xml_list_parser ct pc
end;;



class ['k,'v] xml_hash_parser ct pc =
object
  inherit xml_parser

  val mutable h=Hashtbl.create 2
(*  method get_val n=(DynArray.get frms n : 'a) *)
  method get_hash=h
  method tag=""
  method parse_attr k v=()
  method parse_child k v=
    match k with
      | r when r=ct -> let p=pc() in p#parse v;
	  let r=p#get_val in
	  Hashtbl.add h (fst r:'k) (snd r:'v)
      | _ -> ()
end;;

class ['v] xml_stringhash_parser ct pc=
object
  inherit [string,'v] xml_hash_parser ct pc
end;;




