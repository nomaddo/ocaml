(* TODO: limit duplication numbers *)
open Asttypes
open Ident
open Typedtree

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

let (>>) e r = r := e::!r
let (@|) f x = f x

(* for print dupfun_table *)
let rec str_of_literal = function
  | I -> "#I"
  | F -> "#F"
  | P -> "#P"

and str_of_suffix suffix =
  List.map str_of_literal suffix
  |> List.fold_left (fun str e -> str ^ e) ""

and print_of_suffix (suffix: suffix) =
  str_of_suffix suffix
  |> print_string

and print_of_suffix_cell {suffix; is_used} =
  print_of_suffix suffix;
  Printf.printf " is_used: %b\n" is_used

and print_dupfun {orig_name; suffixes; gtyvars; stamp; loc} =
  Printf.printf "%s : %d : " orig_name stamp;
  List.iter print_of_suffix_cell suffixes;
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

let rec structure_item str_item =
  let desc =
    match str_item.str_desc with
    | Tstr_value (rec_flag, vbs) ->
      let new_vbs = List.map (value_binding []) vbs
        |> List.flatten in
      Tstr_value (rec_flag, new_vbs)
    | Tstr_eval (exp, attrs) ->
      let new_exp = expression [] exp in
      Tstr_eval (new_exp, attrs)
    | _ as self -> self in
  {str_item with str_desc = desc}

and rename_ident ident suffix =
  {ident with name = ident.name ^ str_of_suffix suffix}

and rename_strloc strloc suffix =
  {strloc with txt = strloc.txt ^ str_of_suffix suffix}

and rename_lidentloc lindentloc suffix =
  let rec add_suffix lident suffix =
    let open Longident in
    match lident with
    | Lident s -> Lident (s ^ (str_of_suffix suffix))
    | Ldot (t, str) ->
      let new_t = add_suffix t suffix in
      Ldot (new_t, str)
    | Lapply (t1, t2) -> assert(false)
  in
  {lindentloc with txt = add_suffix lindentloc.txt suffix}

(* XXX: value_binding return value_binding list. Must be flatten. *)
and value_binding freetyvars vb =
  let ty = vb.vb_pat.pat_type in
  let gtyvars =
    Ctype.free_variables ty
    |> List.filter (fun e -> List.for_all ((<>) e) freetyvars) in
  if gtyvars = [] then [vb] else begin
    let suffixes = make_suffixes @| List.length gtyvars in
    let new_vbs = List.map (map_vb gtyvars freetyvars vb) suffixes in
    (* entry_table vb gtyvars ty; *)
    vb::new_vbs
  end

and map_vb gtyvars freetyvars vb suffix =
  let new_pat = rename_pat suffix vb.vb_pat in
  let new_expr = expression freetyvars vb.vb_expr in
  {vb with vb_pat = new_pat; vb_expr = new_expr}

and expression freetyvars exp =
  let desc =
    match exp.exp_desc with
    | Texp_let (rec_flag, vbs, exp) ->
      let new_vbs = List.map (value_binding freetyvars) vbs
                    |> List.flatten in
      let gtyvars =
        List.map (fun vb -> Ctype.free_variables vb.vb_pat.pat_type) vbs
        |> List.flatten in
      let new_exp = expression (freetyvars @ gtyvars) exp in
      Texp_let (rec_flag, new_vbs, new_exp)
    | _ as self -> self
  in
  {exp with exp_desc = desc}

and rename_pat suffix pat =
  let desc =
    match pat.pat_desc with
    | Tpat_any as self -> self
    | Tpat_var (ident, strloc) ->
      let new_ident = rename_ident ident suffix in
      let new_strloc = rename_strloc strloc suffix
      in
      Tpat_var (new_ident, new_strloc)
    | Tpat_alias (pat, ident, strloc) ->
      let new_pat = rename_pat suffix pat in
      let new_ident = rename_ident ident suffix in
      let new_strloc = rename_strloc strloc suffix
      in
      Tpat_alias (new_pat, new_ident, new_strloc)
    | Tpat_constant constant as self -> self
    | Tpat_tuple pats ->
      Tpat_tuple (List.map (rename_pat suffix) pats)
    | Tpat_construct ((lindentloc:Longident.t loc), cstr, pats) ->
      let new_lidentloc = rename_lidentloc lindentloc suffix in
      let new_pats = List.map (rename_pat suffix) pats in
      Tpat_construct (new_lidentloc, cstr, new_pats)
    | Tpat_variant (label, patopt, row_descref) ->
      let new_patopt =
        match patopt with
        | None -> None
        | Some pat -> Some (rename_pat suffix pat) in
      Tpat_variant (label, new_patopt, row_descref)
    | Tpat_record (list, closed_flag) -> (* (lidentloc, label_desc, pat) *)
      let list = List.map (fun (lidentloc, label_desc, pat) ->
          let new_lidentloc = rename_lidentloc lidentloc suffix in
          let new_pat = rename_pat suffix pat in
          (new_lidentloc, label_desc, new_pat)) list in
      Tpat_record (list, closed_flag)
    | Tpat_array pats ->
      Tpat_array (List.map (rename_pat suffix) pats)
    | Tpat_or (pat1, pat2, row_descopt) ->
      let new_pat1 = rename_pat suffix pat1 in
      let new_pat2 = rename_pat suffix pat2 in
      Tpat_or (new_pat1, new_pat2, row_descopt)
    | Tpat_lazy pat ->
      Tpat_lazy (rename_pat suffix pat) in
  {pat with pat_desc = desc}

let structure str =
  let items = List.map structure_item str.str_items in
  {str with str_items = items}
