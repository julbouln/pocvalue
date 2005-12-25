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


open Value_common;;
open Value_xml;;
open Value_lua;;
open Value_object;;
(** Val system with conversion between OCaml, XML and Lua *)

(** {2 Types} *)

(** Generic val type *)
type val_generic=
    [
    | `Int of int
    | `String of string
    | `Float of float
    | `Bool of bool
    | `Text of string
    | `Nil
    ]
;;



type val_format_t=
  | TValList
  | TValXml
  | TValXmlString
  | TValLua
  | TValLuaString;;

type ('a) val_format=
  | ValList of 'a list
  | ValXml of xml_node
  | ValXmlString of string
  | ValLua of lua_obj
  | ValLuaString of string;;


(** {2 Exceptions} *)

exception Bad_val_type of string;;
exception Val_not_found of string;;

(** {2 Functions} *)

(** {3 XML conversion} *)

let xml_of_val v=
  let on=new xml_node in
    (match v with 
      | `Int i->
	  on#of_list [Tag "val_int";Attribute ("value",string_of_int i)]
      | `String s->
	  on#of_list [Tag "val_string";Attribute ("value",s)]
      | `Float f->
	  on#of_list [Tag "val_float";Attribute ("value",string_of_float f)]
      | `Bool b->
	  on#of_list [Tag "val_bool";Attribute ("value",if b then "true" else "false")]
      | `Text s->
	  on#of_list [Tag "val_text";Text s]
      | `Nil -> 
	  on#of_list [Tag "val_nil"]
    );
    on

let val_of_xml x=
  match (x#tag) with
    | "val_int" -> `Int (int_of_string (x#attrib "value"))
    | "val_string" -> `String (x#attrib "value")
    | "val_float" -> `Float (float_of_string (x#attrib "value"))
    | "val_bool" -> `Bool (match (x#attrib "value") with | "true"->true | _ -> false)
    | "val_text" -> `Text (x#pcdata)
    | _ -> `Nil


(** {3 Lua conversion} *)

let lua_of_val=function
  | `Int i->OLuaVal.Number (float_of_int i) 
  | `String s->OLuaVal.String s
  | `Float f->OLuaVal.Number f
  | `Bool s->OLuaVal.Number (if s then 1. else 0.)
  | `Text s->OLuaVal.String s
  | `Nil->OLuaVal.Nil
;; 

let val_of_lua=function
  | OLuaVal.Number f->
      if float (truncate f)=f then
	`Int (truncate f)
      else
	`Float f
  | OLuaVal.String s->`String s
  | OLuaVal.Nil -> `Nil
  | _ -> `Nil
;;

(** {3 Val conversion} *)

let int_of_val=function
  | `Int v->v
  | `String v->int_of_string v
  | `Float v->int_of_float v
  | _->raise (Bad_val_type "int");;

let string_of_val=function
  | `String v->v
  | `Text v->v
  | `Int v->string_of_int v
  | `Float v->string_of_float v
  | _->raise (Bad_val_type "string");;

let float_of_val=function
  | `Int v->float_of_int v
  | `Float v->v
  | _->raise (Bad_val_type "float");;

let bool_of_val=function
  | `Bool v->v
  | _->raise (Bad_val_type "bool");;


let text_of_val=function
  | `String v->v
  | `Text v->v
  | `Int v->string_of_int v
  | `Float v->string_of_float v
  | _->raise (Bad_val_type "text");;



(** {2 Classes} *)

(** Ocaml & Lua & XML interface class *)
class ['a] val_handler (xmlfrom:'a->xml_node) (xmlto:xml_node->'a) (luafrom:'a->OLuaVal.value) (luato:OLuaVal.value->'a)=
object(self)
  inherit generic_object


  method from_format (fmt:('a) val_format)=
    match fmt with
      | ValList l ->self#from_list l
      | ValXml x->self#from_xml x
      | ValXmlString x->self#from_xml_string x
      | ValLua l->self#from_lua l
      | ValLuaString s->self#from_lua_string s

  method to_format (fmt_t:val_format_t)=
    match fmt_t with
      | TValList->ValList (self#to_list())
      | TValXml->ValXml (self#to_xml)
      | TValXmlString->ValXmlString (self#to_xml_string)
      | TValLua->ValLua (self#to_lua)
      | TValLuaString->ValLuaString (self#to_lua_string)


(** OCaml part *)

  val mutable vals=DynArray.create()
  method add_val (n:'a) (v:'a)=
    DynArray.add vals (n,v)

  method set_val (n:'a) (v:'a)=
    if self#is_val n then (
      let ni=ref 0 in
	DynArray.iteri (
	  fun i (nn,vv) ->
	    if nn=n then ni:=i;
	) vals;
	DynArray.set vals !ni (n,v)
    )
    else self#add_val n v

  method get_val (n:'a)=
    let ni=ref (-1) in
      DynArray.iteri (
	  fun i (nn,vv) ->
	    if nn=n then ni:=i;
	) vals;
      if !ni<>(-1) then
	(snd (DynArray.get vals !ni))
      else (
	raise (Val_not_found (string_of_val n))
      )

  method del_val (n:'a)=
    let ni=ref (-1) in
      DynArray.iteri (
	  fun i (nn,vv) ->
	    if nn=n then ni:=i;
	) vals;
      if !ni<>(-1) then
	(DynArray.delete vals !ni)


  method is_val (n:'a)=
    let r=ref false in
      DynArray.iteri (
	fun i (nn,vv) ->
	  if nn=n then r:=true;
      ) vals;    
      !r

  method foreach_val f=
    let g (n,v)=f n v in
    DynArray.iter g vals

(** append val 'a from vh to val 'a self : work for string and text*)
  method append (v:'a) (vh:('a) val_handler)=
    if self#is_val v && vh#is_val v then (
      let sv=self#get_val v and
	  vv=vh#get_val v in
      let rv=
	match (sv,vv) with
	  | (`String v1,`String v2) -> `String (v2^v1)
	  | (`Text v1,`String v2) -> `Text (v2^v1)
	  | (`Text v1,`Text v2) -> `Text (v2^v1)
	  | _ ->`Nil
      in
	self#set_val v rv;
    );
  (** add vh val to self *)
  method merge (vh:('a) val_handler)=
    vh#foreach_val (
      fun k v->
	if self#is_val k=false then
	  self#add_val k v
    )

  (** replace self val with vh one *)
  method flatten (vh:('a) val_handler)=
    vh#foreach_val (
      fun k v->
	self#set_val k v
    )

(* FIXME : cause a segfault! *)
  (** if self val = vh val then delete in self *)
  method sub (vh:('a) val_handler)=
    vh#foreach_val (
      fun k v->
	if self#is_val k then (
	  if v=self#get_val k then 
	    self#del_val k 
	)
	else
	  self#add_val k v
    )


(*
  val mutable vals=Hashtbl.create 2

  method add_val (n:'a) (v:'a)=
    Hashtbl.add vals n v
  method set_val (n:'a) (v:'a)=
    if self#is_val n then
      Hashtbl.replace vals n v
    else self#add_val n v
  method get_val n=
    (try
       Hashtbl.find vals n 
     with Not_found ->
       raise (Val_not_found (string_of_val n)))
  method is_val n=Hashtbl.mem vals n
  method clear()=vals<-Hashtbl.create 2
  method foreach_val f=
    Hashtbl.iter f vals
    *)


  method from_named_list (l:('a*'a) list)=
    List.iter (
      fun (k,v)->
	self#set_val k v
    ) l;

  method from_list (l:('a) list)=
    let i=ref 0 in
    List.iter (
      fun (v)->
	self#set_val (`Int !i) v;
	i:= !i+1;
    ) l;

  method to_list ()=
    let a=DynArray.create() in
    self#foreach_val (
      fun n v->
	DynArray.add a v
    );
      DynArray.to_list a

(** XML part *)

(* NOT IMPLEMENTED *)
  method from_xml_string s=
    ()

  method from_xml x=
    self#set_id x#tag;
    let childs=x#children in
    let i=ref 0 in
    List.iter (
      fun c->
	let v=xmlto c in
	let nm=
	  (try
	     (`String (c#attrib "name")) 
	   with Xml_node_no_attrib v->
	     i:= !i+1;`Int (!i-1)) in
	  self#set_val (nm) v;

    ) childs;

  method to_xml_string=
    let x=self#to_xml in
      x#to_string
 
  method to_xml=
    let n=new xml_node in
      n#set_tag self#get_id;
      self#foreach_val (
	fun k v ->	  
	  let cn=xmlfrom v in
	    cn#add_attrib ("name",string_of_val k);
	    n#add_child cn;
	    
      );
      n

(** Lua part *)

  method from_lua_string str =
    let lo=new lua_obj in
      ignore(lo#parse str);
      self#from_lua ( lo);
      

  method from_lua (t:lua_obj)=
    Luahash.iter (
      fun k v->
	let ak=luato k and
		  av=luato v in
	  self#set_val ak av;
    ) t#to_table;

  method to_lua_string=
    let tbl=self#to_lua#to_table in
	  let str=ref "" in
	    str := (!str^self#get_id^"={");
	    Luahash.iter (
	      fun k v->
		let ak=luato k and
		    av=luato v in
		  (match ak with
		     | `String s-> str := (!str^string_of_val ak^"=")		    
		     | _ -> ()
		  );
		  str:= ( !str^
			    (match av with
			       | `String s -> ("\""^s^"\"")
			       | v -> string_of_val v
			    )
			);
		  str:= (!str^";");
	    ) tbl;
	    str := (!str^"}");
	    !str


  method to_lua=
    let lo=new lua_obj in
      self#foreach_val (
	fun k v ->
	  lo#set_val (luafrom k) (luafrom v)
      );
      lo


end;;

class val_generic_handler=
object
  inherit [val_generic] val_handler xml_of_val val_of_xml lua_of_val val_of_lua 
end;;


