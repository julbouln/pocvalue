(* Linked Hashtbl
 * Copyright (C) 2004 Jesse D. Guardiani
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *)

(*
 * ----------------------------------------------------------------------------
 * linkedHashtbl.ml - Linked Hashtbl
 *           Author - Jesse D. Guardiani
 *          Created - 2004/08/20
 *         Modified - 2004/08/27
 * ----------------------------------------------------------------------------
 *
 *  ChangeLog
 *  ---------
 *
 *  2004/08/27 - JDG
 *  ----------------
 *  - Modified to use Dllist instead of Dustin Sallings' LinkedList module.
 *  - Added fold_left, fold_right, and find_all.
 *
 *  2004/08/20 - JDG
 *  ----------------
 *  - Created
 * ----------------------------------------------------------------------------
 * Copyright (C) 2004 Jesse D. Guardiani
 * Contact: <jesse@wingnet.net>
 * ----------------------------------------------------------------------------
 *)

(** Linked Hashtbl

    This module implements a Linked Hashtbl. It uses a Doubly Linked List to
    track key insertion order. All looping functions walk the Hashtbl keys in
    the order they were inserted.
*)

type ('a, 'b) t = { mutable h:('a, ('a * 'b) Dllist.node_t) Hashtbl.t;
                    mutable l: ('a * 'b) Dllist.node_t option };;


(**
   [create i] Create a Linked Hashtbl with initial size [i]. For best results,
   [i] should be on the order of the expected number of elements that will be in
   the table. The table grows as needed, so [n] is just an initial guess.  
   This is an O(N) operation.
*)
let create i = { h = Hashtbl.create i; l = None };;


(**
   [clear tbl] Empty [tbl].  This is an O(1) operation.
*)
let clear tbl =
        Hashtbl.clear tbl.h;
        tbl.l <- None;
;;


(**
   [add tbl k v] adds a binding of [k] to [v] in [tbl]. Previous bindings for
   [k] are not removed, but simply hidden. That is, after performing [remove
   tbl k], the previous binding for [k], if any, is restored. (Same behavior as
   with association lists.)  This is an O(1) operation.
*)
let add tbl k v =
        let n = begin
                match tbl.l with
                  None   -> let n = (Dllist.create (k,v)) in
                            tbl.l <- Some n;
                            n
                | Some l -> Dllist.append (Dllist.prev l) (k,v)
                end in
        Hashtbl.add tbl.h k n
;;


(**
   [copy tbl] Return a copy of [tbl].  This is an O(N) operation.
*)
let copy tbl =
        match tbl.l with
          None   -> { h = Hashtbl.copy tbl.h; l = None }
        | Some l -> { h = Hashtbl.copy tbl.h; l = Some (Dllist.copy l) };
;;


(**
   [find tbl k] returns the current binding of [k] in [tbl], or raises
   [Not_found] if no such binding exists.  This is an O(1) operation.
*)
let find tbl k =
        snd (Dllist.get (Hashtbl.find tbl.h k))
;;


(**
   [find_all tbl k] returns the list of all data associated with [k] in [tbl].
   The current binding is returned first, then the previous bindings, in
   reverse order of introduction in the table.  This is an O(N) operation for
   the number of duplicate bindings of [k] in [tbl].
*)
let find_all tbl k =
        List.map (fun n -> snd (Dllist.get n)) (Hashtbl.find_all tbl.h k)
;;


(**
   [mem tbl k] checks if [k] is bound in [tbl].  This is an O(1) operation.
*)
let mem tbl k =
        Hashtbl.mem tbl.h k
;;


(**
   [remove tbl k] removes the current binding of [k] in [tbl], restoring the previous
   binding if it exists. It does nothing if [k] is not bound in [tbl].  This is
   an O(1) operation.
*)
let remove tbl k =
        try
                let n = Hashtbl.find tbl.h k in
                match tbl.l with
                  None   ->
                        (* this should never happen *)
                        failwith "LinkedHashtbl: inconsistent state in remove"
                | Some l ->
                        if l == n then
                                let next = Dllist.drop n in
                                if next == n then
                                        tbl.l <- None
                                else tbl.l <- Some next
                        else Dllist.remove n;
                        Hashtbl.remove tbl.h k;
        with Not_found -> ()
;;


(**
   [replace tbl k v] replaces the current binding of [k] in [tbl] by a binding
   of [k] to [v]. If [k] is unbound in [tbl], a binding of [k] to [v] is added
   to [tbl]. This is functionally equivalent to [remove tbl k] followed by
   [add tbl k v], except the order of elements does not change.  This is an
   O(1) operation.
*)
let replace tbl k v =
        try
                let n = Hashtbl.find tbl.h k in
                Dllist.set n (k,v)
        with Not_found -> add tbl k v
;;


(**
   [iter f tbl] applies [f] to all bindings in [tbl]. [f] receives the key as
   first argument, and the associated value as second argument. Each binding is
   presented exactly once to [f]. Bindings are passed to [f] in the order in
   which they were created.  This is an O(N) operation.
*)
let iter f tbl =
        match tbl.l with
          None   -> ()
        | Some l -> Dllist.iter (fun n -> f (fst n) (snd n)) l
;;


(**
   [fold_left f tbl init] computes [f ( ... (f (f init k1 d1) k2 d2 ... kN dN)],
   where [k1 ... kN] are the keys of all bindings in [tbl], and [d1 ... dN] are
   the associated values. Each binding is presented exactly once to [f].
   Bindings are passed to [f] in the order in which they were created.  This is
   an O(N) operation.
*)
let fold_left f init tbl =
        match tbl.l with
          None   -> init
        | Some l -> Dllist.fold_left (fun i (k,v) -> f i k v) init l        
;;


(**
   [fold_right f tbl init] computes [(f kN dN ... (f k1 d1 init)...)], where
   [k1 ... kN] are the keys of all bindings in [tbl], and [d1 ... dN] are the
   associated values. Each binding is presented exactly once to [f]. Bindings
   are passed to [f] in the reverse order in which they were created.  This
   is an O(N) operation.
*)
let fold_right f tbl init =
        match tbl.l with
          None   -> init
        | Some l -> Dllist.fold_right (fun (k,v) i -> f k v i) l init        
;;


(**
   [length tbl] returns the number of bindings in [tbl]. Multiple bindings are
   counted multiply, so [length] gives the number of times [iter] calls its
   first argument.  This is an O(1) operation.
*)
let length tbl =
        ExtHashtbl.Hashtbl.length tbl.h
;;
