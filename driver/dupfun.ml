open TypedtreeMap
open Types

type literal = I | F | P
type suffix = literal list
type suffix_cell = {suffix: suffix; mutable is_used: bool}

(* TODO: XXX: rename suffix  *)

(* bool : whether the suffixed function name is used
   gtyvars : Types.type_expr.id list
   stamp : Ident.t.stamp
   location : Location.t for debug
*)
type dupfun =
  {orig_name: string;
   suffixes: suffix_cell list;
   gtyvars: int list;
   stamp: int;
   loc: Location.t}

let dupfun_table : dupfun list ref = ref []

let (>>) e r = r := e::r

(* for print dupfun_table *)
let rec str_of_literal = function
  | I -> "#I"
  | F -> "#F"
  | P -> "#P"

and print_of_suffix suffix =
  List.map str_of_literal suffix
  |> List.concat ""
  |> print_endline

and print_dupfun {orig_name; suffixes; gtyvars; stamp; loc} =
  Printf.printf "%s : %d : " orig_name stamp;
  List.iter print_of_suffix suffixes;
  print_int_list gtyvars;
  Location.print Format.std_formatter loc

and print_int_list = function
  | [] -> print_endline ""
  | x::xs -> Printf.printf "%d " x; print_int_list xs

(* for duplication *)
let make_suffixes num =
  let rec make num acc =
    match num with
    | 0 -> acc
    | n ->
      acc
      |> List.map (fun l -> [I::l; F::l; P::l])
      |> List.flatten
      |> make (n - 1)
  in
  make num [[]]

(* auxiliary functions   *)
let idents = ref []

let rec bound_idents_with_type pat =
  match pat.pat_desc with
  | Tpat_var (id,s) -> idents := (id,s,pat.pat_type) :: !idents
  | Tpat_alias(p, id, s) ->
      bound_idents_with_type p; idents := (id,s,p.pat_type) :: !idents
  | Tpat_or(p1, _, _) ->
      bound_idents_with_type p1
  | d -> Typedtree.iter_pattern_desc bound_idents_with_type d

let let_bound_idents

(* for definition of duplication module *)
module Duplicate :  MapArgument = struct
  include DefaultMapArgument

  let enter_expression exp =
    match exp.exp_desc with
    | Texp_let (_, vbindings, exp) ->
      List.map (fun vbinding ->
        Typedtree.map_pattern_desc dup_pattern_desc vbinding.vb_pat
      )

module Id_map = TypedtreeMap.MakeMap(TypedtreeMap.DefaultMapArgument)
