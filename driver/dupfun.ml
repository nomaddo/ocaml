open Asttypes
open Ident
open Typedtree

type literal = I | F | P
type suffix = literal list
type suffix_cell = {suffix: suffix; mutable is_used: bool}

(* TODO: XXX: rename suffix_cell  *)

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

let max_size = 3

(* for print dupfun_table *)
let rec str_of_literal = function
  | I -> "#I"
  | F -> "#F"
  | P -> "#P"

and str_of_suffix suffix =
  List.map str_of_literal suffix
  |> List.fold_left (fun str e -> str ^ e) ""

and print_suffix (suffix: suffix) =
  str_of_suffix suffix
  |> print_string

and print_suffix_cell {suffix; is_used} =
  print_suffix suffix;
  Printf.printf " is_used: %b\n" is_used

and print_dupfun {orig_name; suffixes; gtyvars; stamp; loc} =
  Printf.printf "%s : %d : " orig_name stamp;
  List.iter print_suffix_cell suffixes;
  print_int_list gtyvars;
  Location.print Format.std_formatter loc

and print_int_list = function
  | [] -> print_endline ""
  | x::xs -> Printf.printf "%d " x; print_int_list xs

(* print type *)

let rec str_of_path = function
  | Path.Pident id -> id.Ident.name
  | Path.Pdot (t, str, int) -> str ^ "." ^ (str_of_path t)
  | Path.Papply (t1, t2) -> ""

and str_of_type type_expr =
  let open Types in
  match type_expr.desc with
  | Tlink e -> str_of_type e
  | Tvar stropt  ->
     let s =
       match stropt with
       | Some str -> str ^ " "
       | None -> "V" in
     s ^ "(" ^ (string_of_int type_expr.id) ^ ")"
  | Tarrow (l, tyer1, tyer2, _) ->
     str_of_type tyer1
     ^ " -> "
     ^ str_of_type tyer2
  | Ttuple ls ->
     List.map (fun tyer -> str_of_type tyer ^ ", ") ls
     |> List.fold_left (fun a b -> a ^ b) ""
  | Tconstr (path, tyer_list, _) ->
     "("
     ^ (List.map (fun tyer -> str_of_type tyer ^ ", ") tyer_list
        |> List.fold_left (fun a b -> a ^ b) "")
     ^ ") "
     ^ (str_of_path path)
  | Tobject (tyer, opref) -> ""
  | _ -> "p_other"

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

let rename_ident ident suffix =
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
(* XXX: freetyvars and gtyvars are int list! (Types.type_expr.id) *)
let rec value_binding freetyvars vb =
  let ty = vb.vb_pat.pat_type in
  let gtyvars =
    Ctype.free_variables ty
    |> List.map (fun e -> e.Types.id)
    |> List.filter (fun i -> List.for_all ((<>) i) freetyvars) in
  let name_stamps =
    Typedtree.let_bound_idents [vb]
    |> List.map (fun ident -> (ident.name, ident.stamp)) in
  let () = begin
    List.iter (fun (s, i) -> Printf.printf "%s %d\n" s i) name_stamps;
    print_string "gtyvars: "; print_int_list gtyvars;
    print_string "freetyvars: "; print_int_list freetyvars;
    Printf.printf "%s\n\n" @| str_of_type ty
  end in
  let size =
    if List.length gtyvars <= max_size
    then List.length gtyvars
    else begin
      Format.eprintf "DEBGU: too big:@.";
      List.iter (fun (s, i) -> Printf.eprintf "%s %d@." s i) name_stamps;
      Format.eprintf "@.";
      0
    end in
  if size = 0 then [vb] else begin
    let suffixes = make_suffixes size in
    let new_vbs =
      List.map (map_vb gtyvars freetyvars vb) suffixes in
    (* entry_table vb gtyvars ty; *)
    vb::new_vbs
  end

and map_vb gtyvars freetyvars vb suffix =
  let new_pat = rename_pat suffix vb.vb_pat in
  let new_expr = expression (freetyvars @ gtyvars) vb.vb_expr in
  {vb with vb_pat = new_pat; vb_expr = new_expr}

and expression freetyvars exp =
  let desc =
    match exp.exp_desc with
    | Texp_let (rec_flag, vbs, exp) ->
      let new_vbs = List.map (value_binding freetyvars) vbs
                    |> List.flatten in
      let gtyvars =
        List.map (fun vb -> Ctype.free_variables vb.vb_pat.pat_type) vbs
        |> List.flatten
        |> List.map (fun e -> e.Types.id) in
      let new_exp = expression (freetyvars @ gtyvars) exp in
      Texp_let (rec_flag, new_vbs, new_exp)
    | Texp_function (label, cases, p) ->
        Texp_function (label, List.map (map_case freetyvars) cases, p)
    | Texp_apply (exp, list) ->
        let new_list =
          List.map (fun (l, expopt, optional) ->
              match expopt with
              | None -> (l, expopt, optional)
              | Some exp ->
                  (l, Some (expression freetyvars exp), optional)) list in
        Texp_apply (expression freetyvars exp, new_list)
    | Texp_match (exp, cases1, cases2, partial) ->
        let new_cases1 = List.map (map_case freetyvars) cases1 in
        let new_cases2 = List.map (map_case freetyvars) cases2 in
        let new_exp = expression freetyvars exp in
        Texp_match (new_exp, new_cases1, new_cases2, partial)
    | Texp_try (exp, cases) ->
        let new_exp = expression freetyvars exp in
        let new_cases = List.map (map_case freetyvars) cases in
        Texp_try (new_exp, new_cases)
    | Texp_tuple exps ->
        Texp_tuple (List.map (expression freetyvars) exps)
    | Texp_construct (lidentloc, cons_desc, exps) ->
        let new_exps = List.map (expression freetyvars) exps in
        Texp_construct (lidentloc, cons_desc, new_exps)
    | Texp_variant (label, expopt) ->
        let new_expopt = match expopt with
          | None -> None
          | Some exp -> Some (expression freetyvars exp) in
        Texp_variant (label, new_expopt)
    | Texp_record (list, expopt) ->
        let new_list = List.map (fun (lidentloc, label_desc, exp) ->
            let new_exp = expression freetyvars exp in
            (lidentloc, label_desc, new_exp)) list in
        let new_expopt = match expopt with
          | None -> None
          | Some exp -> Some (expression freetyvars exp) in
        Texp_record (new_list, new_expopt)
    | Texp_field (exp, lidentloc, label_desc) ->
        Texp_field (expression freetyvars exp, lidentloc, label_desc)
    | Texp_setfield (exp1, lidentloc, label_desc, exp2) ->
        let new_exp1 = expression freetyvars exp1 in
        let new_exp2 = expression freetyvars exp2 in
        Texp_setfield (new_exp1, lidentloc, label_desc, new_exp2)
    | Texp_array exps ->
        Texp_array (List.map (expression freetyvars) exps)
    | Texp_ifthenelse (exp1, exp2, expopt) ->
        let new_exp1 = expression freetyvars exp1 in
        let new_exp2 = expression freetyvars exp2 in
        let new_expopt = match expopt with
          | None -> None
          | Some exp -> Some (expression freetyvars exp) in
        Texp_ifthenelse (new_exp1, new_exp2, new_expopt)
    | Texp_sequence (exp1, exp2) ->
        Texp_sequence(
          expression freetyvars exp1,
          expression freetyvars exp2)
    | Texp_while (exp1, exp2) ->
        let new_exp1 = expression freetyvars exp1 in
        let new_exp2 = expression freetyvars exp2 in
        Texp_while (new_exp1, new_exp2)
    | Texp_for (ident, ppat, exp1, exp2, dflag, exp3) ->
        Texp_for (ident, ppat,
                  expression freetyvars exp1,
                  expression freetyvars exp2,
                  dflag,
                  expression freetyvars exp3)
    | Texp_send (exp, meth, expopt) ->
        let new_expopt = match expopt with
          | None -> None
          | Some exp -> Some (expression freetyvars exp) in
      Texp_send (expression freetyvars exp,
                 meth, new_expopt)
    | Texp_setinstvar (p1, p2, strloc, exp) ->
      Texp_setinstvar (p1, p2, strloc, expression freetyvars exp)
    | Texp_assert exp ->
        Texp_assert (expression freetyvars exp)
    | Texp_lazy exp ->
        Texp_lazy (expression freetyvars exp)
    | Texp_ident _ | Texp_constant _  | Texp_new _ | Texp_instvar _
    | Texp_override _ | Texp_letmodule _
    | Texp_object _ | Texp_pack _ as self -> self
  in
  {exp with exp_desc = desc}

and map_case freetyvars c =
  let c_guard = (function
      | None -> None
      | Some exp -> Some (expression freetyvars exp)) c.c_guard in
  let c_rhs = expression freetyvars c.c_rhs in
  {c with c_guard; c_rhs}

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
    | Tpat_construct (lindentloc, cstr, pats) ->
      let new_lidentloc = rename_lidentloc lindentloc suffix in
      let new_pats = List.map (rename_pat suffix) pats in
      Tpat_construct (new_lidentloc, cstr, new_pats)
    | Tpat_variant (label, patopt, row_descref) ->
      let new_patopt =
        match patopt with
        | None -> None
        | Some pat -> Some (rename_pat suffix pat) in
      Tpat_variant (label, new_patopt, row_descref)
    | Tpat_record (list, closed_flag) ->
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
      Tpat_lazy (rename_pat suffix pat)
  in
  {pat with pat_desc = desc}

let structure_item str_item =
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

let structure str =
  let items = List.map structure_item str.str_items in
  {str with str_items = items}
