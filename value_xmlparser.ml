open Value_xml;;

(** Xml parsers *)

(** virtual xml parser *)
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



(** {3 General parsers} *)

(** int parser of form <tag a="int"> *)
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

(** string parser of form <tag a="string"> *)
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


(** point parser of form <tag x="int" y="int"> *)
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

(** size parser of form <tag w="int" h="int"> *)
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


(** color parser of form <tag r="int" g="int" b="int"> *)
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

(** list parser  *)
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

(** list of int parser *)
class xml_intlist_parser ct pc=
object
  inherit [int,xml_int_parser] xml_list_parser ct pc
end;;

(** list of string parser *)
class xml_stringlist_parser ct pc=
object
  inherit [string,xml_string_parser] xml_list_parser ct pc
end;;


(** hashtbl parser *)
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

(** string hashtble parser *)
class ['v] xml_stringhash_parser ct pc=
object
  inherit [string,'v] xml_hash_parser ct pc
end;;


open Value_val;;

class xml_val_generic_list_parser otag=
object(self)
  inherit xml_parser
  val mutable vals=new val_generic_handler

  method parse_attr k v=()

  method get_val=vals

  method parse_child k v=
    match k with
      | tag when tag=otag ->
	    vals#from_xml v
      | _ -> ()
end;;


