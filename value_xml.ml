(*
    Battle For Rashitoul - The ultimate strategy/arcade game
    Copyright (C) 2003,2004 POC 

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


(** Xml parse in an object way *)

exception Bad_xml_node;;

let xml_reduce n (f:Xml.xml->bool)=
  let childd=DynArray.create() in
  List.iter (
    fun x->(
      match x with
	| Element v -> if (f x) then DynArray.add childd x
	| _ ->()
      )
  ) (Xml.children n);
    match n with
      | Element (t,attrs,childs)->Element (t,attrs,DynArray.to_list childd)
      | _ -> raise Bad_xml_node ;;
    

exception Xml_node_attr_not_found of string;;
exception Xml_node_child_not_found of string;;

class xml_node (n:Xml.xml)=
object(self)
  val mutable node=n
  val mutable tag="none"
  val mutable pcdata=""
  val mutable attrs=Hashtbl.create 2
  val mutable children=Hashtbl.create 2

  method set_tag t=tag<-t
  method get_tag=tag

  method set_pcdata t=pcdata<-t
  method get_pcdata=pcdata

  method add_attr k v=
    Hashtbl.add attrs (String.lowercase k) v;
  method get_attr k=
    (try 
       Hashtbl.find attrs k
     with Not_found -> raise (Xml_node_attr_not_found k))
  method foreach_attr f=Hashtbl.iter f attrs

  method add_child k v=Hashtbl.add children k v
  method get_child k=
    (try 
       Hashtbl.find children k
     with Not_found -> raise (Xml_node_child_not_found k))
  method foreach_child f=Hashtbl.iter f children

  initializer
    (* set tag *)
    self#set_tag (Xml.tag node);

    (* add attributes *)
    List.iter (
      fun (k,v)->(
	self#add_attr k v)
    ) (Xml.attribs node);
    
    (* add children *)
    List.iter (
      fun x->(
	match x with
	  | Element v -> self#add_child (Xml.tag x) (new xml_node x)
	  | PCData v -> self#set_pcdata v
      )
    ) (Xml.children node)
      

  method set_node n=node<-n
  method get_node=node

end;;



class virtual xml_parser=
object(self)
  method tag=""
  method virtual parse_attr: string->string->unit
  method virtual parse_child: string->xml_node->unit

  method parse (n:xml_node)= 
    n#foreach_attr self#parse_attr;
    n#foreach_child self#parse_child;
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

(* NEW *)
(*

arbre_type.xml (pousse)
<game_object name="arbre">
<header>
  <inherit file="decor_type.xml"/>
</header>
<args>
</args>


</game_object>

arbre_tropical.xml (climat=tropical, qualite du bois=4)
<game_object name="arbre_tropical">
<header>
  <inherit file="arbre_type.xml"/>
</header>
<args>
</args>
<properties>
<val_string name="climat" value="tropical"/>
<val_named_list name="bois">
 <val_int name="qualite">4</val_int>
</val_named_list>
</properties>
</game_object>

decor->arbre->arbre_fruitier->cocotier
            ->arbre_tropical->

*)


class virtual xml_object=
object(self)

(* general *)
  method get_tag x=Xml.tag x
  method get_attrib x n=Xml.attrib x n
  method get_attribs x=Xml.attribs x
  method get_children x=Xml.children x
  method get_pcdate x=Xml.pcdata x

  method foreach_attrib x f=
    List.iter f (self#get_attribs x);

  method foreach_child x f=
    List.iter f (self#get_children x);


  method merge x1 x2=
    let rec m xe1 xe2=
      match (xe1,xe2) with
	| (Element(t1,a1,c1),Element(t2,a2,c2)) when t1=t2 && a1=a2->
	    let ncl=ref c1 in
	      List.iter (
		fun c->
		  List.iter (
		    fun cc ->
		      ncl:= !ncl@(m cc c);
		  ) c1;		  
	      ) c2;
	      
	      [(Element(t1,a1,!ncl))]
		
	| (o1,o2) -> [] in
      m x1 x2

(* to xml *)
  method virtual to_xml : Xml.xml
  method to_xml_string=
    let x=self#to_xml in
      Xml.to_string x
  method save_to_file f=
    let fo=open_out f in
      output_string fo (self#to_xml_string);
      close_out fo;

(* from xml *)
  method virtual from_xml : Xml.xml -> unit
  method from_xml_string s=
    let x=Xml.parse_string s in
      self#from_xml x
  method load_from_file f=
    let x=Xml.parse_file f in
      self#from_xml x


end;;








