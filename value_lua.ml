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

(** Oo style lua-ml *)

module T  = Lua.Lib.Combine.T1 (Luaiolib.T)
module WT = Lua.Lib.WithType (T)
module C  = Lua.Lib.Combine.C4 (Luaiolib.Make (T.TV1)) (Luacamllib.Make(T.TV1))
                               (WT(Luastrlib.M)) (WT(Luamathlib.M))
			       
module I = Lua.MakeInterp (Lua.Parser.MakeStandard) (Lua.MakeEval (T) (C))



module OLuaVal = I.Value
let ( **-> ) = OLuaVal. ( **-> )
let ( **->> ) x y = x **-> OLuaVal.result y

(** lua classes *)

class lua_interp=
object(self)
val mutable vals=DynArray.create();
val mutable interp=I.mk()

initializer
  self#set_global_val "randomize" (OLuaVal.efunc (OLuaVal.int **->> OLuaVal.int) randomize)

method set_global_val n f=
self#add_val n f;
self#register_vals_global()

method set_module_val m n f=
self#add_val n f;
self#register_vals_module m

method get_val f=
I.getglobal interp (OLuaVal.String f)

method add_val n f=
DynArray.add vals (n,f)

method register_vals_global()=
 I.register_globals
 (DynArray.to_list vals)
 interp;
 DynArray.clear vals;
 
method register_vals_module m=
 I.register_module m
 (DynArray.to_list vals)
 interp;
 DynArray.clear vals;
		
method parse e= I.dostring interp e

end;;


class lua_obj=
object(self)
  val mutable interp=new lua_interp
  val mutable vals=Luahash.create (fun a b->a=b) 2
    
  method del_val k=
    Luahash.remove vals (k);
    self#update_interp();

  method set_val k (v:OLuaVal.value)=
    Luahash.replace vals ~key:(k) ~data:v;
    self#update_interp();
      
  method get_val k=
    self#update_vals();
    Luahash.find vals (k)

  method exec_val_fun k args=
    let v=self#get_val k in
     (match v with
	| OLuaVal.Function (v,f)-> f args
	| _ -> [OLuaVal.Nil]
     )
  method parse e=
    interp#parse e;

  method update_interp()=
    interp#set_global_val "self" (OLuaVal.Table self#to_table);
    
  method update_vals()=
    let tv=interp#get_val "self" in
      match tv with
	| OLuaVal.Table tbl->vals<-tbl
	| _ ->()

  method to_table=vals
  method from_table v=vals<-v;self#update_interp()

  method set_obj_val (nm:string) (o:lua_obj)=
    self#set_val (OLuaVal.String nm) (OLuaVal.Table o#to_table)


end;;

class lua_object=
object
  val mutable lua=new lua_obj
  method get_lua=lua

  val mutable lua_script=""
  method set_lua_script l=lua_script<-l
  method get_lua_script=lua_script

  method lua_init()=
    lua#parse lua_script

  method lua_parent_of nm (obj:lua_object)=
    obj#get_lua#set_obj_val "parent" lua;
    lua#set_obj_val nm obj#get_lua;

end;;


(*
let test=new lua_interp in
test#set_module_val "Test" "test" (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.string) Sys.getenv);
test#set_module_val "Test" "test2" (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.string) (fun v->v));
test#set_global_val  "a" (OLuaVal.String "bla2");
let a=(test#get_val "a") in
print_string (OLuaVal.to_string a);


test#parse "
function test3 () 
print (\"test3\") 
end
print(\"bla\");
print (Test.test (\"PWD\"));
print (Test.test2 (\"PWD\"));
print (a)
";

test#parse "test3()";
*)
