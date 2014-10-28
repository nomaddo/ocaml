open Asttypes
open Ident
open Typedtree
open Format

(*
   duplicationとname manglingをいっぺんにやる
   上から下にただ読んでいくのではなく、
   let value_bindings in exp ではexpを見てからvbsを見るため、
   慎重に探索順序を実装する
   expの中身を見て、expで使われてる通りだけローカルでしか使われない
   多相関数を複製することにする

   まずはsignatureを気にせず実装するぞー
*)


type literal = I | F | P
type suffix = literal list

type suffix_cell = {
  suffix: suffix;
  context: (int * literal) list
}

type dupfun = {
  name: string;
  stamp: int;
  mutable suffixes: suffix_cell list ;
  gtyvars: int list;
}

module Dmap = struct
  let dmap : (Path.t * dupfun) list ref  = ref []
  let update_dmap v =
    dmap := v :: !dmap
  let find_value p =
    try
      Some (List.assoc p !dmap)
    with Not_found -> None
  let find_ident ident =
    try
      Some (List.find (fun (p,_) -> Path.head p = ident) !dmap)
    with Not_found -> None
       | Assert_failure _ as exn -> begin
           printf "assertion fail in Path.head@.";
           raise exn
         end
  let find_name name =
    try
      Some (List.find (fun (p, _) -> Path.last p = name) !dmap)
    with Not_found -> None
end

module Context =
  Map.Make(struct type t = int let compare = compare end)

let merge_context a b =
  Context.merge (fun k x y ->
      match x, y with
      | None, None -> None
      | Some e, _ -> Some e
      | None, Some e -> Some e) a b

let rename_ident ident suffix =
  {ident with name = ident.name ^ str_of_suffix suffix}
let rename_strloc strloc suffix =
  {strloc with txt = strloc.txt ^ str_of_suffix suffix}
let rename_lidentloc lindentloc suffix =
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

let rec rename_pat suffix pat =
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

let expression context exp =
  try
    let desc =
      match exp.exp_desc with
      | Texp_ident (path, lidentloc, vdesc) as self -> begin
          match path with
          | Path.Papply _ -> Texp_ident (path, lidentloc, vdesc)
          | _ ->
              try
                let {Ident.name; stamp} = Path.head path in
                match  ..... with
                | None -> self
                | Some dupfun ->
                    let general_type = dupfun.Dupfun.ty in
                    let gtyvars = dupfun.Dupfun.gtyvars in
                    let env = exp.exp_env
                              |> Envaux.env_of_only_summary in
                    let suffix =
                      inference env gtyvars context general_type exp.exp_type in
                    let lidentloc = name_mangle lidentloc suffix in
                    Texp_ident (path, lidentloc, vdesc)
              with Fail_to_unify
                 | Poly_variant -> Texp_ident (path, lidentloc, vdesc)
        end
      | Texp_let (rec_flag, vbs, exp) ->
          let new_exp = expression new_context exp in
          let new_vbs, new_contexts =
            List.map (value_binding context) vbs
            |> List.split in
          let new_context = List.flatten new_contexts in
          Texp_let (rec_flag, new_vbs, new_exp)
      | Texp_function (label, cases, p) ->
          Texp_function (label, List.map (map_case context) cases, p)
      | Texp_apply (exp, list) ->
          let new_list =
            List.map (fun (l, expopt, optional) ->
                match expopt with
                | None -> (l, expopt, optional)
                | Some exp ->
                    (l, Some (expression context exp), optional)) list in
          Texp_apply (expression context exp, new_list)
      | Texp_match (exp, cases1, cases2, partial) ->
          let new_cases1 = List.map (map_case context) cases1 in
          let new_cases2 = List.map (map_case context) cases2 in
          let new_exp = expression context exp in
          Texp_match (new_exp, new_cases1, new_cases2, partial)
      | Texp_try (exp, cases) ->
          let new_exp = expression context exp in
          let new_cases = List.map (map_case context) cases in
          Texp_try (new_exp, new_cases)
      | Texp_tuple exps ->
          Texp_tuple (List.map (expression context) exps)
      | Texp_construct (lidentloc, cons_desc, exps) ->
          let new_exps = List.map (expression context) exps in
          Texp_construct (lidentloc, cons_desc, new_exps)
      | Texp_variant (label, expopt) ->
          let new_expopt = match expopt with
            | None -> None
            | Some exp -> Some (expression context exp) in
          Texp_variant (label, new_expopt)
      | Texp_record (list, expopt) ->
          let new_list = List.map (fun (lidentloc, label_desc, exp) ->
              let new_exp = expression context exp in
              (lidentloc, label_desc, new_exp)) list in
          let new_expopt = match expopt with
            | None -> None
            | Some exp -> Some (expression context exp) in
          Texp_record (new_list, new_expopt)
      | Texp_field (exp, lidentloc, label_desc) ->
          Texp_field (expression context exp, lidentloc, label_desc)
      | Texp_setfield (exp1, lidentloc, label_desc, exp2) ->
          let new_exp1 = expression context exp1 in
          let new_exp2 = expression context exp2 in
          Texp_setfield (new_exp1, lidentloc, label_desc, new_exp2)
      | Texp_array exps ->
          Texp_array (List.map (expression context) exps)
      | Texp_ifthenelse (exp1, exp2, expopt) ->
          let new_exp1 = expression context exp1 in
          let new_exp2 = expression context exp2 in
          let new_expopt = match expopt with
            | None -> None
            | Some exp -> Some (expression context exp) in
          Texp_ifthenelse (new_exp1, new_exp2, new_expopt)
      | Texp_sequence (exp1, exp2) ->
          Texp_sequence(
            expression context exp1,
            expression context exp2)
      | Texp_while (exp1, exp2) ->
          let new_exp1 = expression context exp1 in
          let new_exp2 = expression context exp2 in
          Texp_while (new_exp1, new_exp2)
      | Texp_for (ident, ppat, exp1, exp2, dflag, exp3) ->
          Texp_for (ident, ppat,
                    expression context exp1,
                    expression context exp2,
                    dflag,
                    expression context exp3)
      | Texp_send (exp, meth, expopt) ->
          let new_expopt = match expopt with
            | None -> None
            | Some exp -> Some (expression context exp) in
          Texp_send (expression context exp,
                     meth, new_expopt)
      | Texp_setinstvar (p1, p2, strloc, exp) ->
          Texp_setinstvar (p1, p2, strloc, expression context exp)
      | Texp_assert exp ->
          Texp_assert (expression context exp)
      | Texp_lazy exp ->
          Texp_lazy (expression context exp)
      | Texp_constant _  | Texp_new _ | Texp_instvar _
      | Texp_override _ | Texp_letmodule _
      | Texp_object _ | Texp_pack _ as self -> self
    in
    {exp with exp_desc = desc}
  with exn -> begin
      if flag
      then eprintf "expression: at [%a]" Location.print exp.exp_loc;
      raise exn
    end

let choice_vb str vbs =
  let ids_vb = List.map (fun vb ->
      let ids = Typedtree.pat_bound_idents vb.vb_pat
                |> List.split |> fst
      in (ids, vb)) vbs
  in
  List.find (fun (ids, vb) -> List.exists ((=) str) ids) |> snd

let rec dup_loop c vbs already = function
  | (str,suffix,context)::xs
    when not (List.exists ((=) str) already) ->
      let vb = choice_vb str vbs in
      let pat = rename_pat dup.suffix vb.vb_pat in
      let defined =
        Typedtree.pat_bound_idents pat
        |> List.split |> fst |> List.map Ident.name in
      let defined = defined @ already in
      let exp, dups = expression (c @ context) vb.vb_expr in
      let vb = {vb with vb_pat = pat; vb_expr = exp} in
      vb :: dup_loop c vbs defined (dups @ xs)
  | _::xs -> dup_loop c vbs already_defined
  | [] -> []

let rec value_bindings ftvs vbs =
  List.filter (fun  !map


let rec structure_item str_item =
  let desc =
    match str_item.str_desc with
    | Tstr_value (rec_flag, vbs) ->
        let new_vbs = value_bindings [] vbs in
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

let structure str =
  let items =                   (* 逆向きに探索 *)
    List.rev str.str_items
    |> List.map structure_item
    |> List.rev in
  {str with str_items = items}

let structure str sg =
  let str = structure str in
  if !Clflags.dump_typedtree
  then List.iter print_dupfun !dupfun_table;
  str
