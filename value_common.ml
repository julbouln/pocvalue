
(** common useful funcs *)

Random.self_init();;

(** get random number *)
let randomize n= 
 (Random.int n)

(** Generic core objects *)

(** the most lowlevel object *)
exception Object_id_not_set;;

class generic_object=
object
  val mutable id=None
  (** set the id of this object *)
  method set_id (i:string)=id<-(Some i)
  (** get the id of this object *)
  method get_id=
    match id with
      | Some i->i
      | None -> raise Object_id_not_set

(*
  method print_info()=
    print_string ("POCCORE: object info");print_newline();
    match id with
      | Some i->print_string (" * id: "^i);print_newline();
      | None ->print_string (" * id: not set!");print_newline();
*)
end;;


exception Object_not_found of string;;


let list_of_hash h=
  let l=ref [] in
    Hashtbl.iter (
      fun k v->
	l:=List.append !l [(k,v)];
    ) h;
    !l;;


(** object handler using hashtbl *)
class ['a] generic_object_handler=
object(self)
  val mutable objs=Hashtbl.create 2

  method add_object (id:string option) (o:'a)=
    let nid=
      (match id with
	 | Some nid->(nid)
	 | None ->("object"^string_of_int (Oo.id o)))
       in
      o#set_id nid;
      Hashtbl.add objs nid o;nid

  method replace_object id o=
    Hashtbl.replace objs (id) o

  method is_object id=
    Hashtbl.mem objs (id)

  method get_object id=
    (try
       Hashtbl.find objs (id)
     with Not_found -> raise (Object_not_found id))

  method delete_object id=
    Hashtbl.remove objs (id)

  method rename_object id nid=
    let o=self#get_object id in
      ignore(self#add_object (Some nid) o);
      self#delete_object id;
    
  method foreach_object f=
    Hashtbl.iter f objs


  method foreach_object_sorted s f=
    let ol=list_of_hash objs in
    let sl=List.sort s ol in
    let hf(v,k)=f v k in
      List.iter hf sl;

end;;


(** object handler using array *)
class ['a] generic_object_handler2=
object(self)
  val mutable objs=DynArray.create()

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
