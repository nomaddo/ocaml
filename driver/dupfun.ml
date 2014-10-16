open Asttypes
open Ident
open Typedtree
open Format

type literal = I | F | P
type suffix = literal list
type suffix_cell = {
  suffix: suffix;
  context: (int * literal) list;
  mutable is_used: bool
}


(* TODO: XXX: rename suffix_cell  *)

(* orig_name : original name
   ty : type
   suffixes : suffix_cell list
   gtyvars : Types.type_expr.id list
   stamp : Ident.t.stamp
   loc : Location.t for debug
*)

type dupfun =
  {orig_name: string;
   ty : Types.type_expr;
   suffixes: suffix_cell list;
   gtyvars: int list;
   stamp: int;
   loc: Location.t}

let dupfun_table : dupfun list ref = ref []

let (>>) e r = r := e::!r
let (@|) f x = f x

let max_size = 3

(* module DMap = Map.Make(struct type t = Ident.t let compare = compare end) *)

(* let map = ref DMap.empty *)

module G = struct (* for get bound idents with type *)
  let idents = ref([]: (Ident.t * Types.type_expr) list)

  let rec bound_idents pat =
    match pat.pat_desc with
    | Tpat_var (id,s) ->
        idents := (id, pat.pat_type) :: !idents
    | Tpat_alias(p, id, s ) ->
        bound_idents p;
        idents := (id, pat.pat_type) :: !idents
    | Tpat_or(p1, _, _) ->
        (* Invariant : both arguments binds the same variables *)
        bound_idents p1
    | d -> Typedtree.iter_pattern_desc bound_idents d

  let pat_bound_idents pat =
    idents := []; bound_idents pat;
    let res = !idents in idents := []; res

  let rev_let_bound_idents vbs =
    idents := [];
    List.iter (fun vb -> bound_idents vb.vb_pat) vbs;
    let res = !idents in idents := []; res

  let let_bound_idents vbs =
    List.rev(rev_let_bound_idents vbs)

  let name_stamp_type vbs =
    let (ids, types) = let_bound_idents vbs
                       |> List.split in
    let names = List.map (fun id -> id.Ident.name) ids
    and stamps = List.map (fun id -> id.Ident.stamp) ids in
    (names, stamps, types)
end

module SigUtil = struct
  let sg : Types.signature option ref = ref (Obj.magic ())
  let exists = ref false

  let set_flag = function
    | None -> exists := false
    | Some _ -> exists := false

  (* let find_value sg id = *)
  (*   let find = function *)
  (*     | Tsig_value (ident, vdesc) -> Ident.same id ident *)
  (*     | _ -> false in *)
  (*   try Some (List.find find sg) *)
  (*   with Not_found -> None *)
end

let rec iter3 f l1 l2 l3 =
  match (l1, l2, l3) with
    ([], [], []) -> ()
  | (a1::l1, a2::l2, a3::l3) -> f a1 a2 a3; iter3 f l1 l2 l3
  | (_, _, _) -> invalid_arg "List.iter3"

let rec print_list s p ppf = function
    | [] -> fprintf ppf ""
    | x::[] -> fprintf ppf "%a" p x
    | x::xs -> fprintf ppf "%a %s %a"
                 p x s (print_list s p) xs

(* entry table *)

let entry_table ~orig_name ~suffixes ~gtyvars ~ty ~stamp ~loc =
  let contexts =
    List.map (fun suffix -> List.combine gtyvars suffix) suffixes in
  let suffixes =
    List.map2 (fun suffix context ->
      {suffix; context; is_used=false}) suffixes contexts in
  {orig_name; suffixes; gtyvars; stamp; loc; ty}
  >> dupfun_table

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

and print_dupfun {orig_name; ty; suffixes; gtyvars; stamp; loc} =
  Format.printf "$$$$$$$$$$$$$$$$$$$$$$$$$@.";
  Printf.printf "name: %s, stamp: %d\n" orig_name stamp;
  List.iter print_suffix_cell suffixes;
  printf "type: %a" print_type ty;
  print_string "gtyvars: "; print_int_list gtyvars;
  (* Location.print Format.std_formatter loc; *)
  Format.printf "$$$$$$$$$$$$$$$$$$$$$$$$$@."

and print_int_list = function
  | [] -> print_endline ""
  | x::xs -> Printf.printf "%d " x; print_int_list xs

(* print type *)

and print_type ppf ty =
  let open Types in
  match ty.desc with
  | Tvar stropt -> begin
      match stropt with
      | Some str -> fprintf ppf "V(%s[%d])" str ty.id
      | None -> fprintf ppf "V[%d]" ty.id
    end
  | Tarrow (label, ty1, ty2, _) -> begin
      match label with
      | "" -> fprintf ppf "(%a -> %a)"
                print_type ty1 print_type ty2
      | _ -> fprintf ppf "([~%s]:%a -> %a)"
               label print_type ty1 print_type ty2
    end
  | Ttuple tys ->
      fprintf ppf "(%a)" (print_list "*" print_type) tys
  | Tconstr (path, tys, _) -> begin
      try
        fprintf ppf "[%a] %a"
          (print_list "," print_type) tys Printtyp.path path
      with Assert_failure _ ->
        fprintf ppf "[%a] %a"
          (print_list "," print_type) tys Printtyp.path path
    end
  | Tobject (ty, optref) ->
      let p ppf (path, tys) =
        fprintf ppf "%a [%a]"
          Path.print path
          (print_list "," print_type) tys in
      let p_optref ppf optref =
        match !optref with
        | None -> fprintf ppf "None"
        | Some l -> p ppf l in
      fprintf ppf "Tobject (%a,[%a])"
        print_type ty p_optref optref
  | Tfield (str, fkind, ty1, ty2) ->
      fprintf ppf "Tfield (%s, %a, %a, %a)" str
        Types.print_field_kind fkind
        print_type ty1
        print_type ty2
  | Tnil ->
      fprintf ppf "Tnil"
  | Tlink ty -> print_type ppf ty
  | Tsubst ty -> fprintf ppf "Tsubst (%a)" print_type ty
  | Tvariant row_desc -> Printtyp.raw_type_expr ppf ty
  | Tunivar stropt ->
      let s =
        match stropt with
        | None -> "None"
        | Some str -> sprintf "Some(%s)" str in
      fprintf ppf "Tunivar (%s)" s
  | Tpoly (ty, tys) ->
      fprintf ppf "Tpoly (%a, [%a])"
        print_type ty
        (print_list ";" print_type) tys
  | Tpackage (path, lidents, tys) ->
      fprintf ppf "Tpackage (%a, [%a], [%a])"
        Path.print path
        (print_list ";" Longident.print) lidents
        (print_list ";" print_type) tys

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

let rec check_ignore_case ty =
  let open Types in
  let check_list l =
    List.map check_ignore_case l
    |> List.fold_left (fun a b -> a && b) true in
  match ty.desc with
  | Tvar _ -> true
  | Tarrow (_, ty1,ty2,_) ->
      check_ignore_case ty1 && check_ignore_case ty2
  | Ttuple tyl -> check_list tyl
  | Tconstr (_,tyl, _) -> check_list tyl
  | Tobject _ -> false
  | Tfield _ -> false
  | Tnil -> false
  | Tlink ty -> check_ignore_case ty
  | Tsubst _ -> false
  | Tvariant _ -> false
  | Tunivar _ -> false
  | Tpoly _ -> false
  | Tpackage _ -> false

(* XXX: value_binding return value_binding list. Must be flatten. *)
(* XXX: freetyvars and gtyvars are int list! (Types.type_expr.id list) *)
let rec value_binding freetyvars vb =
  let ty = vb.vb_pat.pat_type in
  let gtyvars =
    Ctype.free_variables ty
    |> List.map (fun e -> e.Types.id)
    |> List.filter (fun i -> List.for_all ((<>) i) freetyvars) in
  let names, stamps, types = G.name_stamp_type [vb] in
  let size =
    if List.length gtyvars <= max_size
    then List.length gtyvars
    else begin
      Format.eprintf "DEBUG: too big: %d@." @| List.length gtyvars;
      List.iter2 (Format.eprintf "%s %d@.") names stamps;
      Format.eprintf "type: %a@." print_type ty;
      Format.eprintf "@.";
      0
    end in
  if size = 0 then [vb] else
  if not @| check_ignore_case ty
  then begin
    if true
    then printf "ignore_case: %a@."
        (print_list "," (fun ppf -> fprintf ppf "%s")) names;
    [vb]
  end else
    begin
    let suffixes = make_suffixes size in
    let new_vbs =
      List.map (map_vb gtyvars freetyvars vb) suffixes in
    let loc = vb.vb_pat.pat_loc in
    iter3 (fun name stamp ty->
        entry_table
          ~orig_name:name
          ~suffixes:suffixes
          ~gtyvars:gtyvars
          ~stamp:stamp
          ~loc:loc
          ~ty:ty) names stamps types;
    map_vb gtyvars freetyvars vb [] :: new_vbs
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
    | Tstr_module mb ->
        let mb_expr = module_expr mb.mb_expr in
        Tstr_module {mb with mb_expr = mb_expr}
    | _ as self -> self
  in
  {str_item with str_desc = desc}

and module_expr mod_expr =
  match mod_expr.mod_desc with
  | Tmod_structure str ->
      let new_str = structure str in
      {mod_expr with mod_desc = Tmod_structure new_str}
  | Tmod_constraint (mod_expr2, _, _, _) ->
      module_expr mod_expr2
  | Tmod_functor (ident, strloc, modtyopt, mod_expr2) ->
      let new_mod_expr = module_expr mod_expr2 in
      let mod_desc =
        Tmod_functor
          (ident, strloc, modtyopt, new_mod_expr) in
      {mod_expr with mod_desc}
  | _ -> mod_expr

and structure str =
  let items = List.map structure_item str.str_items in
  {str with str_items = items}

let structure str sg =
  let str = structure str in
  SigUtil.sg := sg;
  SigUtil.set_flag sg;
  if !Clflags.dump_typedtree
  then List.iter print_dupfun !dupfun_table;
  str
