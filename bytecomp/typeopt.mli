(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Auxiliaries for type-based optimizations, e.g. array kinds *)

module IntS : (Set.S with type elt = int)

val has_base_type : Typedtree.expression -> Path.t -> bool
val maybe_pointer : Typedtree.expression -> bool
val array_kind
  : IntS.t Stack.t -> Typedtree.expression -> Lambda.array_kind
val array_pattern_kind
  : ?stack: IntS.t Stack.t -> Typedtree.pattern -> Lambda.array_kind
val bigarray_kind_and_layout :
      Typedtree.expression -> Lambda.bigarray_kind * Lambda.bigarray_layout
