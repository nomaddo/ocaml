(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* A variant of the "lambda" code with direct / indirect calls explicit
   and closures explicit too *)

open Asttypes
open Lambda

type function_label = string

type ustructured_constant =
  | Uconst_float of float
  | Uconst_int32 of int32
  | Uconst_int64 of int64
  | Uconst_nativeint of nativeint
  | Uconst_block of int * uconstant list
  | Uconst_float_array of float list
  | Uconst_string of string

and uconstant =
  | Uconst_ref of string * ustructured_constant
  | Uconst_int of int
  | Uconst_ptr of int

type ulambda =
    Uvar of Ident.t
  | Uspecialized of ulambda * kind_map
  | Uconst of uconstant
  | Udirect_apply of function_label * ulambda list * Debuginfo.t
  | Ugeneric_apply of ulambda * ulambda list * Debuginfo.t
  | Uclosure of ufunction list * ulambda list
  | Uoffset of ulambda * int
  | Ulet of Ident.t * ulambda * ulambda
  | Uletrec of (Ident.t * ulambda) list * ulambda
  | Uprim of primitive * ulambda list * Debuginfo.t
  | Uswitch of ulambda * ulambda_switch
  | Ustringswitch of ulambda * (string * ulambda) list * ulambda option
  | Ustaticfail of int * ulambda list
  | Ucatch of int * Ident.t list * ulambda * ulambda
  | Utrywith of ulambda * Ident.t * ulambda
  | Uifthenelse of ulambda * ulambda * ulambda
  | Usequence of ulambda * ulambda
  | Uwhile of ulambda * ulambda
  | Ufor of Ident.t * ulambda * ulambda * direction_flag * ulambda
  | Uassign of Ident.t * ulambda
  | Usend of meth_kind * ulambda * ulambda * ulambda list * Debuginfo.t

and ufunction = {
  label  : function_label;
  arity  : int;
  params : Ident.t list;
  body   : ulambda;
  dbg    : Debuginfo.t
}

and ulambda_switch =
  { us_index_consts: int array;
    us_actions_consts : ulambda array;
    us_index_blocks: int array;
    us_actions_blocks: ulambda array}

(* Description of known functions *)

type function_description =
  { fun_label: function_label;          (* Label of direct entry point *)
    fun_arity: int;                     (* Number of arguments *)
    mutable fun_closed: bool;           (* True if environment not used *)
    mutable fun_inline: (Ident.t list * ulambda) option;
    mutable fun_float_const_prop: bool  (* Can propagate FP consts *)
  }

(* Approximation of values *)

type value_approximation =
    Value_closure of function_description * value_approximation
  | Value_tuple of value_approximation array
  | Value_unknown
  | Value_const of uconstant
  | Value_global_field of string * int

(* Comparison functions for constants.  We must not use Pervasives.compare
   because it compares "0.0" and "-0.0" equal.  (PR#6442) *)

let compare_floats x1 x2 =
  Int64.compare (Int64.bits_of_float x1) (Int64.bits_of_float x2)

let rec compare_float_lists l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | [], _::_ -> -1
  | _::_, [] -> 1
  | h1::t1, h2::t2 ->
      let c = compare_floats h1 h2 in
      if c <> 0 then c else compare_float_lists t1 t2

let compare_constants c1 c2 =
  match c1, c2 with
  | Uconst_ref(lbl1, c1), Uconst_ref(lbl2, c2) -> String.compare lbl1 lbl2
      (* Same labels -> same constants.
         Different labels -> different constants, even if the contents
           match, because of string constants that must not be
           reshared. *)
  | Uconst_int n1, Uconst_int n2 -> Pervasives.compare n1 n2
  | Uconst_ptr n1, Uconst_ptr n2 -> Pervasives.compare n1 n2
  | Uconst_ref _, _ -> -1
  | Uconst_int _, Uconst_ref _ -> 1
  | Uconst_int _, Uconst_ptr _ -> -1
  | Uconst_ptr _, _ -> 1

let rec compare_constant_lists l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | [], _::_ -> -1
  | _::_, [] -> 1
  | h1::t1, h2::t2 ->
      let c = compare_constants h1 h2 in
      if c <> 0 then c else compare_constant_lists t1 t2

let rank_structured_constant = function
  | Uconst_float _ -> 0
  | Uconst_int32 _ -> 1
  | Uconst_int64 _ -> 2
  | Uconst_nativeint _ -> 3
  | Uconst_block _ -> 4
  | Uconst_float_array _ -> 5
  | Uconst_string _ -> 6

let compare_structured_constants c1 c2 =
  match c1, c2 with
  | Uconst_float x1, Uconst_float x2 -> compare_floats x1 x2
  | Uconst_int32 x1, Uconst_int32 x2 -> Int32.compare x1 x2
  | Uconst_int64 x1, Uconst_int64 x2 -> Int64.compare x1 x2
  | Uconst_nativeint x1, Uconst_nativeint x2 -> Nativeint.compare x1 x2
  | Uconst_block(t1, l1), Uconst_block(t2, l2) ->
      let c = t1 - t2 (* no overflow possible here *) in
      if c <> 0 then c else compare_constant_lists l1 l2
  | Uconst_float_array l1, Uconst_float_array l2 ->
      compare_float_lists l1 l2
  | Uconst_string s1, Uconst_string s2 -> String.compare s1 s2
  | _, _ -> rank_structured_constant c1 - rank_structured_constant c2
                (* no overflow possible here *)

let rec subst_array_kind map ulam =
  let subst = subst_array_kind map in
  match ulam with
  | Uprim (prim, args, dinfo) ->
    begin match prim with
    | Pmakearray (Ptvar i as k) ->
        let k = Simplif.gen_kind map i k in
        Uprim (Pmakearray k, List.map subst args, dinfo)
    | Parraylength (Ptvar i as k)  ->
        let k = Simplif.gen_kind map i k in
        Uprim (Parraylength k, List.map subst args, dinfo)
    | Parrayrefu (Ptvar i as k)  ->
        let k = Simplif.gen_kind map i k in
        Uprim (Parrayrefu k, List.map subst args, dinfo)
    | Parraysetu (Ptvar i as k)  ->
        let k = Simplif.gen_kind map i k in
        Uprim (Parraysetu k, List.map subst args, dinfo)
    | Parrayrefs (Ptvar i as k)  ->
        let k = Simplif.gen_kind map i k in
        Uprim (Parrayrefs k, List.map subst args, dinfo)
    | Parraysets (Ptvar i as k)  ->
        let k = Simplif.gen_kind map i k in
        Uprim (Parraysets k, List.map subst args, dinfo)
    | _ -> Uprim (prim, List.map subst args, dinfo)
    end
  | Uvar _ as u -> u
  | Uspecialized (u, kind_map) -> Uspecialized (subst u, kind_map)
  | Uconst _ as u -> u
  | Udirect_apply (function_label, us, dinfo) ->
    Udirect_apply (function_label, List.map subst us, dinfo)
  | Ugeneric_apply (u, us, dinfo) ->
    Ugeneric_apply (subst u, List.map subst us, dinfo)
  | Uclosure (ufuncs, ulams) ->
    let subst_fun ufun = {ufun with body = subst ufun.body} in
    Uclosure (List.map subst_fun ufuncs, List.map subst ulams)
  | Uoffset (ulam, i) -> Uoffset (subst ulam, i)
  | Ulet (id, u1, u2) ->
    Ulet (id, subst u1, subst u2)
  | Uletrec (idus, u) ->
    let subst_pair (id, u) = (id, subst u) in
    Uletrec (List.map subst_pair idus, subst u)
  | Uswitch (ulam, ulamswitch) ->
    let subst_switch sw =
      {sw with us_actions_blocks = Array.map subst sw.us_actions_blocks;
               us_actions_consts =Array.map subst sw.us_actions_consts} in
    Uswitch (subst ulam, subst_switch ulamswitch)
  | Ustringswitch (ulam, l, uopt) ->
      let map_pair = (fun (s, u) -> (s, subst u)) in
      let map_opt = function None -> None | Some e -> Some (subst e) in
      Ustringswitch (subst ulam, List.map map_pair l, map_opt uopt)
  | Ustaticfail (i, ulams) ->
      Ustaticfail (i, List.map subst ulams)
  | Ucatch (i, ids, ulam1, ulam2) ->
      Ucatch (i, ids,  subst ulam1, subst ulam2)
  | Utrywith (ulam1, id, ulam2) ->
      Utrywith (subst ulam1, id, subst ulam2)
  | Uifthenelse (ulam1, ulam2, ulam3) ->
      Uifthenelse (subst ulam1, subst ulam2, subst ulam3)
  | Usequence (ulam1, ulam2) ->
      Usequence (subst ulam1, subst ulam2)
  | Uwhile (ulam1, ulam2) -> Uwhile (subst ulam1, subst ulam2)
  | Ufor (id, ulam1, ulam2, df, ulam3) ->
      Ufor (id, subst ulam1, subst ulam2, df, subst ulam3)
  | Uassign (id, ulam) ->
      Uassign (id, subst ulam)
  | Usend (meth_kind, ulam1, ulam2, ulams, dinfo) ->
      Usend (meth_kind, subst ulam1, subst ulam2, List.map subst ulams, dinfo)
