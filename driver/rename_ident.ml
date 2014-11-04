open Typedtree
open Types
open Format

exception Fail_to_unify
exception Poly_variant

let flag = true

let table = ref (Obj.magic 1)

let (>>) e r =
  r := e :: !r

let print_type = Dupfun.print_type

let list_map f l =
  let rec map acc f = function
    | x :: xs -> map ((f x) :: acc) f xs
    | [] -> List.rev acc in
  map [] f l

let list_map2 f a b =
  let rec map acc f a b =
    match a, b with
    | x :: xs, y :: ys -> map ((f x y)::acc) f xs ys
    | [] , [] -> List.rev acc
    | [], _ | _, [] -> raise (Invalid_argument "list_map2") in
  map [] f a b

module Unify = struct
  let rec unify_typexpr ty1 ty2 =
    match ty1.desc, ty2.desc with
    | Tvar _, _ ->
        [(ty1.id, ty2)]
    | Tlink ty , _ ->
        unify_typexpr ty ty2
    | _, Tlink ty ->
        unify_typexpr ty1 ty
    | Tarrow (_, tyx, tyy, _),
      Tarrow (_, tyx', tyy', _) ->
        unify_typexpr tyx tyx'
        @ unify_typexpr tyy tyy'
    | Ttuple tylist, Ttuple tylist' ->
        list_map2 unify_typexpr tylist tylist'
        |> List.flatten
    | Tconstr (p1, tylist, _),
      Tconstr (p2, tylist', _) ->
        if p1 = p2
        then list_map2 unify_typexpr tylist tylist'
             |> List.flatten
        else begin
          if flag then begin
            eprintf "unify: type name mismatch\n";
            eprintf "ty1: %a@." print_type ty1;
            eprintf "ty2: %a@." print_type ty2;
          end;
          raise Fail_to_unify
        end
    | Tfield (_, _, tyx, tyy),
      Tfield (_, _, tyx', tyy') ->
      unify_typexpr tyx tyx' @ unify_typexpr tyy tyy'
    | Tvariant r1, Tvariant r2 ->
      raise Poly_variant
    | Tpoly (ty1, _), _ -> unify_typexpr ty1 ty2
    | _, Tpoly (ty2, _) -> unify_typexpr ty1 ty2
    | _ -> begin
        if flag
        then eprintf "Fail Unify:\nty1: %a@.ty2: %a@."
            Printtyp.type_expr ty1
            Printtyp.type_expr ty2
      end; raise Fail_to_unify
end

let str_suffix suffix =
  list_map Dupfun.str_of_literal suffix
  |> String.concat ""

let name_mangle (lidentloc : Longident.t Asttypes.loc) suffix =
  let open Longident in
  let open Asttypes in
  let str_suffix = str_suffix suffix in
  let name_mangle' lident =
    match lident with
    | Lident name -> Lident (name ^ str_suffix)
    | Ldot (t, name) -> Ldot  (t, name ^ str_suffix)
    | Lapply _ -> assert(false)
  in
  {lidentloc with txt = name_mangle' lidentloc.txt}

let str_split str =
  try
    let pos = String.index str '#' in
    (String.sub str 0 pos,
     String.sub str pos (String.length str - pos))
  with Not_found -> (str, "")

let access_table str i =        (* str as orig_name *)
  try
    Some (Hashtbl.find !table (str, i))
  with _ -> None

let get_context name stamp =
  let open Dupfun in
  let get_context' dupfun str_suffix =
    List.find (fun suffix_cell ->
      str_suffix = str_of_suffix suffix_cell.suffix) dupfun.suffixes
    |> (fun x -> x.context)
  in
  let (orig_name, str_suffix) = str_split name in
  match access_table name stamp with
  | Some dupfun -> begin try Some (get_context' dupfun str_suffix) with Not_found -> None end
  | None -> None

let rec unalias_type env ty = (* Env.t -> type_expr -> type_expr *)
  match ty.desc with
  | Tconstr (path, types, abbrev_memo_ref) -> begin
      let types = list_map (unalias_type env) types in
      let desc = Tconstr (path, types, abbrev_memo_ref) in
      let ty = {ty with desc} in
      Ctype.my_nondep_type_rec env ty
    end
  | Tvar _ -> ty
  | Tarrow (l, ty1, ty2, com) ->
    {ty with Types.desc = Tarrow (l, unalias_type env ty1, unalias_type env ty2, com)}
  | Ttuple tyexprs ->
    {ty with Types.desc = Ttuple (list_map (unalias_type env) tyexprs)}
  | Tfield (name, field_kind, ty1, ty2) ->
    {ty with Types.desc = Tfield (name, field_kind, unalias_type env ty1, unalias_type env ty2)}
  | Tlink ty -> unalias_type env ty
  | _ -> ty

let sort gtyvars unif_result =
  list_map (fun id -> try
               (id ,Some (List.assoc id unif_result))
             with _ -> (id, None)) gtyvars

let name_inference context (id, optty) =
  (* XXX: context is id and literal. For example,
     let f#i#f x y = ... and now see ident in "...",
     context has information of x = I, y = F
  *)
  let open Dupfun in
  let ident_name = function
    | "int" -> I
    | "float" -> F
    | "char" -> I
    | "string" -> P
    | _ -> I in
  let rec inference ty =
    let open Path in
    let open Ident in
    match ty.desc with
    | Tarrow _ -> P
    | Ttuple _ -> P
    | Tconstr (path,tylist,_) ->
        if tylist <> [] then P
        else begin match path with
          | Pident ident -> ident_name ident.name
          | _ -> P (* XXX: TODO: fix me *)
        end
    | Tobject _ -> P
    | Tfield _ -> P
    | Tlink ty -> inference ty
    | Tvar _ | Tunivar _ -> begin
        try
          List.assoc ty.id context
        with Not_found -> begin
            (*
            str_of_type ty |> printf "name_inference: %s@.";
            printf "context: ";
            List.iter (fun (i, l) ->
                printf "%d %s, " i (str_of_literal l)) context;
            printf "@.%d@." id;
            *)
            (* assert(false) *)
            I
          end
      end
    | _ -> begin
        if flag then eprintf "inference: Unexpected type [%a]@."
            print_type ty;
        I                       (* XXX : fix me *)
        (* assert(false) *)
      end in
  match optty with
  | None -> I
  | Some ty -> inference ty

let inference env gtyvars context gty ty = (* gty = more general type *)
  (* int list -> type_expr -> type_expr -> suffix *)
  let ty1 = unalias_type env gty in
  let ty2 = unalias_type env ty in
  let unif_result = Unify.unify_typexpr ty1 ty2
                    |> sort gtyvars in (* (int * opt type) list *)
  let suffix = list_map (name_inference context) unif_result in
  suffix

let set_flag scope cells suffix =
  List.find (fun c -> c.Dupfun.suffix = suffix) cells
  |> (fun c -> c.Dupfun.is_used <- true)

let not_papply = function Path.Papply _ -> false | _ -> true

let rec value_binding context scope vb =
  (* context -> vb -> vb * context *)
  let make_new_context name stamp =
    match get_context name stamp with
    | None -> []
    | Some c -> c @ context
  in
  let names, stamps, _ = Dupfun.G.name_stamp_type [vb] in
  let pairs = list_map2 (fun n i -> (n, i)) names stamps in
  (* XXX : inefficient procedures here *)
  let c = list_map2 make_new_context names stamps
          |> List.concat in
  let exp = expression (c @ context) (pairs @ scope) vb.vb_expr in
  let new_vb = {vb with vb_expr = exp} in
  new_vb

and expression context scope exp =
  try
    if !Clflags.mydump then Location.print_loc std_formatter exp.exp_loc;
    let desc =
      match exp.exp_desc with
      | Texp_ident (path, lidentloc, vdesc) as self when not_papply path -> begin
          try
            let {Ident.name; stamp} = Path.head path in
            match access_table name stamp with
            | None -> self
            | Some dup ->
                let general_type = dup.Dupfun.ty in
                let gtyvars = dup.Dupfun.gtyvars in
                let env = exp.exp_env
                          |> Envaux.env_of_only_summary in
                let suffix =
                  inference env gtyvars context general_type exp.exp_type in
                let str_suffix = str_suffix suffix in
                let lidentloc = name_mangle lidentloc suffix in
                if not (List.exists ((=) (name ^ str_suffix, stamp)) scope)
                then set_flag scope dup.Dupfun.suffixes suffix;
                Texp_ident (path, lidentloc, vdesc)
          with Fail_to_unify
             | Poly_variant -> Texp_ident (path, lidentloc, vdesc)
        end
      | Texp_ident _ as self -> self
      | Texp_let (rec_flag, vbs, exp) ->
          let new_vbs =
            list_map (value_binding context scope) vbs in
          let new_exp = expression context scope exp in
          Texp_let (rec_flag, new_vbs, new_exp)
      | Texp_function (label, cases, p) ->
          Texp_function (label, list_map (map_case context scope) cases, p)
      | Texp_apply (exp, list) ->
          let new_list =
            list_map (fun (l, expopt, optional) ->
                match expopt with
                | None -> (l, expopt, optional)
                | Some exp ->
                    (l, Some (expression context scope exp), optional)) list in
          Texp_apply (expression context scope exp, new_list)
      | Texp_match (exp, cases1, cases2, partial) ->
          let new_cases1 = list_map (map_case context scope) cases1 in
          let new_cases2 = list_map (map_case context scope) cases2 in
          let new_exp = expression context scope exp in
          Texp_match (new_exp, new_cases1, new_cases2, partial)
      | Texp_try (exp, cases) ->
          let new_exp = expression context scope exp in
          let new_cases = list_map (map_case context scope) cases in
          Texp_try (new_exp, new_cases)
      | Texp_tuple exps ->
          Texp_tuple (list_map (expression context scope) exps)
      | Texp_construct (lidentloc, cons_desc, exps) ->
          let new_exps = list_map (expression context scope) exps in
          Texp_construct (lidentloc, cons_desc, new_exps)
      | Texp_variant (label, expopt) ->
          let new_expopt = match expopt with
            | None -> None
            | Some exp -> Some (expression context scope exp) in
          Texp_variant (label, new_expopt)
      | Texp_record (list, expopt) ->
          let new_list = list_map (fun (lidentloc, label_desc, exp) ->
              let new_exp = expression context scope exp in
              (lidentloc, label_desc, new_exp)) list in
          let new_expopt = match expopt with
            | None -> None
            | Some exp -> Some (expression context scope exp) in
          Texp_record (new_list, new_expopt)
      | Texp_field (exp, lidentloc, label_desc) ->
          Texp_field (expression context scope exp, lidentloc, label_desc)
      | Texp_setfield (exp1, lidentloc, label_desc, exp2) ->
          let new_exp1 = expression context scope exp1 in
          let new_exp2 = expression context scope exp2 in
          Texp_setfield (new_exp1, lidentloc, label_desc, new_exp2)
      | Texp_array exps ->
          Texp_array (list_map (expression context scope) exps)
      | Texp_ifthenelse (exp1, exp2, expopt) ->
          let new_exp1 = expression context scope exp1 in
          let new_exp2 = expression context scope exp2 in
          let new_expopt = match expopt with
            | None -> None
            | Some exp -> Some (expression context scope exp) in
          Texp_ifthenelse (new_exp1, new_exp2, new_expopt)
      | Texp_sequence (exp1, exp2) ->
          Texp_sequence(
            expression context scope exp1,
            expression context scope exp2)
      | Texp_while (exp1, exp2) ->
          let new_exp1 = expression context scope exp1 in
          let new_exp2 = expression context scope exp2 in
          Texp_while (new_exp1, new_exp2)
      | Texp_for (ident, ppat, exp1, exp2, dflag, exp3) ->
          Texp_for (ident, ppat,
                    expression context scope exp1,
                    expression context scope exp2,
                    dflag,
                    expression context scope exp3)
      | Texp_send (exp, meth, expopt) ->
          let new_expopt = match expopt with
            | None -> None
            | Some exp -> Some (expression context scope exp) in
          Texp_send (expression context scope exp,
                     meth, new_expopt)
      | Texp_setinstvar (p1, p2, strloc, exp) ->
          Texp_setinstvar (p1, p2, strloc, expression context scope exp)
      | Texp_assert exp ->
          Texp_assert (expression context scope exp)
      | Texp_lazy exp ->
          Texp_lazy (expression context scope exp)
      | Texp_constant _  | Texp_new _ | Texp_instvar _
      | Texp_override _ | Texp_letmodule _
      | Texp_object _ | Texp_pack _ as self -> self
    in
    {exp with exp_desc = desc}
  with Stack_overflow as exn -> raise exn
     | exn -> begin
      if flag
      then eprintf "expression: at [%a]" Location.print exp.exp_loc;
      raise exn
    end

and map_case context scope c =
  let c_guard = (function
      | None -> None
      | Some exp -> Some (expression context scope exp)) c.c_guard in
  let c_rhs = expression context scope c.c_rhs in
  {c with c_guard; c_rhs}

let rec structure_item str_item =
  let desc =
    match str_item.str_desc with
    | Tstr_value (rec_flag, vbs) ->
        let new_vbs = list_map (value_binding [] []) vbs in
        Tstr_value (rec_flag, new_vbs)
    | Tstr_eval (exp, attrs) ->
        let new_exp = expression [] [] exp in
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
      let mod_desc = Tmod_functor (ident, strloc, modtyopt, new_mod_expr) in
      {mod_expr with mod_desc}
  | _ -> mod_expr

and structure str =
  let items = list_map structure_item str.str_items in
  {str with str_items = items}

let structure str =
  table := Dupfun.dupfun_table;
  structure str
