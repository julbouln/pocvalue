(*
    pocvalue - data manager
    Copyright (C) 2005 POC 

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

(** Common classes *)

(** common useful funcs *)
Random.self_init();;

(** get random number *)
let randomize n= 
 (Random.int n)

(** convert a hashtbl in list *)
let list_of_hash h=
  let l=ref [] in
    Hashtbl.iter (
      fun k v->
	l:=List.append !l [(k,v)];
    ) h;
    !l;;


(** {2 Exceptions} *)

exception Object_id_not_set;;
exception Object_type_not_set;;
exception Object_not_found of string;;

(** {2 Classes} *)

(** {3 Objects classes} *)

(** the most lowlevel object *)
class generic_object=
object
  (** id as string option *)
  val mutable id=None

  (** set the id of this object *)
  method set_id (i:string)=id<-(Some i)

  (** get the id of this object 
      raise Object_id_not_set if not set
  *)
  method get_id=
    match id with
      | Some i->i
      | None -> raise Object_id_not_set

end;;

(** the same with type *)
class generic_object_typed=
object
  inherit generic_object
  val mutable name=None

  (** set the type of this object *)
  method set_name (i:string)=name<-(Some i)

  (** get the type of this object 
      raise Object_type_not_set if not set
  *)
  method get_name=
    match id with
      | Some i->i
      | None -> raise Object_type_not_set

end;;

(** {3 Objects handlers classes} *)

(** virtual objects handler *)
class virtual ['a] virtual_object_handler=
object
  method virtual clear: unit->unit
  method virtual objs_count: int
  method virtual add_object: string option->'a->string
  method virtual replace_object: string -> 'a-> unit
  method virtual is_object: string -> bool
  method virtual get_object: string -> 'a
  method virtual delete_object: string->unit
  method virtual rename_object: string->string->unit
  method virtual foreach_object: (string->'a->unit)->unit
(*  method virtual foreach_object_sorted: ((string->'a)->(string->'a)->int)->(string->'a->unit)->unit *)
end;;

(** object handler using hashtbl *)
class ['a] generic_object_handler=
object(self)
  inherit ['a] virtual_object_handler
  val mutable objs=Hashtbl.create 2

  (** delete all objects in handler *)
  method clear()=
    Hashtbl.clear objs;

  (** count objects in handler *)
  method objs_count=Hashtbl.length objs

  (** add object in handler 
      if id is None then handler generate an automatic id 
  *)
  method add_object (id:string option) (o:'a)=
    let nid=
      (match id with
	 | Some nid->(nid)
	 | None ->("object"^string_of_int (Oo.id o)))
       in
      o#set_id nid;
      Hashtbl.add objs nid o;nid

  (** replace object with id by new object *)
  method replace_object id o=
    Hashtbl.replace objs (id) o

  (** is object with id exist in handler ? *)
  method is_object id=
    Hashtbl.mem objs (id)

  (** get object with id *)
  method get_object id=
    (try
       Hashtbl.find objs (id)
     with Not_found -> raise (Object_not_found id))

  (** delete object with id *)
  method delete_object id=
    Hashtbl.remove objs (id)

  (** rename object with id to new id *)
  method rename_object id nid=
    let o=self#get_object id in
      ignore(self#add_object (Some nid) o);
      self#delete_object id;
    
  (** iter handler foreach object *)
  method foreach_object f=
    Hashtbl.iter f objs

  (** iter handler foreach object with sort *)
  method foreach_object_sorted s f=
    let ol=list_of_hash objs in
    let sl=List.sort s ol in
    let hf(v,k)=f v k in
      List.iter hf sl;

end;;

(** other implementations of handler, same methods *)

(** object handler using array *)
class ['a] generic_object_handler2=
object(self)
  inherit ['a] virtual_object_handler
  val mutable objs=DynArray.create()

  method objs_count=DynArray.length objs

  method clear()=
    DynArray.clear objs

  method private obj_n id=
    let ni=ref (-1) in
      DynArray.iteri (
	  fun i o ->
	    if o#get_id=id then ni:=i;
	) objs;
      !ni

  method add_object (id:string option) (o:'a)=
    let nid=
      (match id with
	 | Some nid->(nid)
	 | None ->("object"^string_of_int (Oo.id o)))
       in
      o#set_id nid;
      DynArray.add objs (o);nid

  method replace_object id o=
    let ni=self#obj_n id in
      if ni<>(-1) then
	DynArray.set objs ni o
      else
	ignore(self#add_object (Some id) o)

  method is_object id=
    let r=ref false in
      DynArray.iter (
	fun (o) ->
	  if o#get_id=id then r:=true;
      ) objs;    
      !r

  method get_object id=
    let ni=self#obj_n id in
      if ni<>(-1) then
	(DynArray.get objs ni)
      else
	raise (Object_not_found id)

  method delete_object id=
    let ni=self#obj_n id in
      if ni<>(-1) then
	(DynArray.delete objs ni)

  method rename_object id nid=
    let o=self#get_object id in
      ignore(self#add_object (Some nid) o);
      self#delete_object id;
    
  method foreach_object f=
    let g o=f o#get_id o in
    DynArray.iter g objs

(* NOT IMPLEMENTED *)
  method foreach_object_sorted (s:(string*'a)->(string*'a)->int) f=
    self#foreach_object f

end;;


let list_of_linkhash h=
  let l=ref [] in
    LinkedHashtbl.iter (
      fun k v->
	l:=List.append !l [(k,v)];
    ) h;
    !l;;

(** object handler using linkedhashtbl *)
class ['a] generic_object_handler3=
object(self)
  inherit ['a] virtual_object_handler
  val mutable objs=LinkedHashtbl.create 2

  method clear()=
    LinkedHashtbl.clear objs;

  method objs_count=LinkedHashtbl.length objs

  method add_object (id:string option) (o:'a)=
    let nid=
      (match id with
	 | Some nid->(nid)
	 | None ->("object"^string_of_int (Oo.id o)))
       in
      o#set_id nid;
      LinkedHashtbl.add objs nid o;nid

  method replace_object id o=
    LinkedHashtbl.replace objs (id) o

  method is_object id=
    LinkedHashtbl.mem objs (id)

  method get_object id=
    (try
       LinkedHashtbl.find objs (id)
     with Not_found -> raise (Object_not_found id))

  method delete_object id=
    LinkedHashtbl.remove objs (id)

  method rename_object id nid=
    let o=self#get_object id in
      ignore(self#add_object (Some nid) o);
      self#delete_object id;
    
  method foreach_object f=
    LinkedHashtbl.iter f objs

  method foreach_object_sorted s f=
    let ol=list_of_linkhash objs in
    let sl=List.sort s ol in
    let hf(v,k)=f v k in
      List.iter hf sl;

end;;

