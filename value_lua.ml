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

open Value_common;;

(** Lua object representation *)


(** lua-ml related *)

module T  = Lua.Lib.Combine.T1 (Luaiolib.T)
module WT = Lua.Lib.WithType (T)
module C  = Lua.Lib.Combine.C4 (Luaiolib.Make (T.TV1)) (Luacamllib.Make(T.TV1))
                               (WT(Luastrlib.M)) (WT(Luamathlib.M))
			       
module I = Lua.MakeInterp (Lua.Parser.MakeStandard) (Lua.MakeEval (T) (C))

module OLuaVal = I.Value
let ( **-> ) = OLuaVal. ( **-> )
let ( **->> ) x y = x **-> OLuaVal.result y

(** lua classes *)

(** interpret string *)
class lua_interp=
object(self)

(** the interpreter *)
val mutable interp=I.mk()

initializer
  self#set_global_val "randomize" (OLuaVal.efunc (OLuaVal.int **->> OLuaVal.int) randomize)

method set_global_val n f=
 I.register_globals
   [(n,f)]
   interp;

method set_module_val m n f=
 I.register_module m
   [(n,f)]
   interp;

method get_val f=
I.getglobal interp (OLuaVal.String f)

method parse e= I.dostring interp e

end;;


(** lua object part *)

(** exceptions *)

exception Lua_not_found of string;;
exception Lua_bad_type of string;;
exception Lua_error of string*string*string;;

(** convert luaval to string *)
let string_of_luaval=function
  | OLuaVal.String s->s
  | OLuaVal.Number s->string_of_float s
  | OLuaVal.Nil -> "nil"
  | _ -> raise (Lua_bad_type "string");;

(** the lua obj *)
class lua_obj=
object(self)
  val mutable interp=new lua_interp
  val mutable vals=Luahash.create (fun a b->a=b) 2
    
  initializer
    interp#set_global_val "self" (OLuaVal.Table vals); 

  method del_val k=
    Luahash.remove vals (k);

  method set_val k (v:OLuaVal.value)=
    Luahash.replace vals ~key:(k) ~data:v;
      
  method get_val k=
    (try
       Luahash.find vals (k)
     with
	 Not_found -> raise (Lua_not_found (string_of_luaval k)))

  method get_parent=
    let p=new lua_obj in
      p#from_table (
	match (self#get_val (OLuaVal.String "parent")) with
		 | OLuaVal.Table tbl -> tbl
		 | _ -> (raise (Lua_error (self#get_self_id,"parent","parent")))
	      );
      p
	 
  method get_self_id=
    let str=ref [] in
    let rec parent_id p=
      (try 
	 match p with
	   | OLuaVal.Table tbl->
	       let fid=Luahash.find tbl (OLuaVal.String "get_id") in
		 (match fid with
		    | OLuaVal.Function (v,f)-> 
			(try 
			   str:= !str@
			     [string_of_luaval(List.nth (f [OLuaVal.Nil]) 0)];
			   parent_id (Luahash.find tbl (OLuaVal.String "parent"))
			 with
			   |I.Error e->(raise (Lua_error ("?","get_id",e))))
		    | _ -> ()
		 )
	   | _ ->()
       with _ ->()) in
      
      parent_id (self#get_val(OLuaVal.String "parent"));
      let id=self#exec_val_fun (OLuaVal.String "get_id") [OLuaVal.Nil] in 
	
      str:= ["root"]@(List.rev !str)@[string_of_luaval (List.nth id 0)];
      String.concat "." !str;

  method get_fun k=
    let v=self#get_val k in
      (match v with
      | OLuaVal.Function (v,f)-> 
	  (try 
	     f
	   with
	     |I.Error e->(raise (Lua_error (self#get_self_id,string_of_luaval k,e)))
	  )
      | _ -> fun l->[OLuaVal.Nil]
      )
  method exec_val_fun k args=
(*    (try *)
       let v=self#get_val k in
	 (match v with
	    | OLuaVal.Function (v,f)-> 
		(try 
		   f args
		 with
		   |I.Error e->(raise (Lua_error (self#get_self_id,string_of_luaval k,e)))
		)
	    | _ -> [OLuaVal.Nil]
	 )
(*     with
	 Lua_not_found v->[OLuaVal.Nil])*)

  method parse e=
    (try
      interp#parse e;
     with
       |I.Error e->

	   (raise (Lua_error (self#get_self_id,"",e)))
	   
    )

  method to_table=vals
  method from_table v=vals<-v

  method set_obj_val (nm:string) (o:lua_obj)=
    self#set_val (OLuaVal.String nm) (OLuaVal.Table o#to_table)


end;;

(** to be integrated in objects *)
class virtual lua_object=
object(self)
  val mutable lua=new lua_obj
  method get_lua=lua

  method virtual get_id:string

  val mutable lua_script=""
  method set_lua_script l=lua_script<-l
  method get_lua_script=lua_script

  method lua_init()=

(*    print_string ("LUA: init "^self#get_id);print_newline(); *)
    lua#set_val (OLuaVal.String "get_id") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.string) (fun()->self#get_id));
    lua#set_val (OLuaVal.String "get_global_id") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.string) (fun()->lua#get_self_id));
    ignore(lua#parse lua_script)

  method lua_parent_of nm (obj:lua_object)=
    obj#get_lua#set_obj_val "parent" lua;
    lua#set_obj_val nm obj#get_lua;
    

end;;

