open Typedtree
open Types

exception Fail_to_unify

let table = ref (Obj.magic 1)

let (>>) e r =
  r := e :: !r

let print_type = Printtyp.type_expr Format.std_formatter

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
      List.map2 unify_typexpr tylist tylist'
      |> List.flatten
    | Tconstr (_, tylist, _),
      Tconstr (_, tylist', _) ->
      List.map2 unify_typexpr tylist tylist'
      |> List.flatten
    | Tfield (_, _, tyx, tyy),
      Tfield (_, _, tyx', tyy') ->
      unify_typexpr tyx tyx'
      @ unify_typexpr tyy tyy'
    | Tvariant r1, Tvariant r2 ->
      unify_row r1 r2
    | _ ->
      Format.eprintf "Error:@.ty1: %a@.ty2: %a@."
        Printtyp.type_expr ty1
        Printtyp.type_expr ty2;
      raise Fail_to_unify

  and unify_row r1 r2 =
    let rec _uni = function
      | [] -> []
      | (label, rowf1)::xs ->
        let m = List.find_all (fun (l, _) -> l = label) r2.row_fields in
        let _ = assert (List.length m = 1) in
        let (_, rowf2) = List.hd m in
        (row_field rowf1 rowf2) :: _uni xs
    in
    _uni r1.row_fields
    |> List.flatten

  and row_field f1 f2 =
    match f1, f2 with
    | Rabsent, _ -> assert(false)
    | _, Rabsent -> assert(false)
    | Rpresent None, _ -> []
    | Rpresent (Some tyexpr), Rpresent None -> assert(false)
    | Rpresent (Some tyexpr1), Rpresent (Some tyexpr2) ->
      unify_typexpr tyexpr1 tyexpr2
    | Rpresent (Some tyexpr), Reither (_, lty, _, _) ->
      Format.eprintf "DEBUG: Reither occuar@."; []
    (* assert(List.length lty = 1); *)
    (* let h::_ = tyexprlist in *)
    (* unify_typexpr tyexpr h *)
    | Reither (_, lty1, _, _), Reither (_, lty2, _, _) ->
      Format.eprintf "DEBUG: Reither occuar@."; []
    (* assert(List.length lty1 = 1); *)
    (* assert(List.length lty2 = 1); *)
    | _ -> assert(false)

end

let name_mangle (lidentloc : Longident.t Asttypes.loc) suffix =
  let open Longident in
  let open Asttypes in
  let str_suffix = List.map Dupfun.str_of_literal suffix
    |> String.concat "" in
  let rec name_mangle' lident =
    match lident with
    | Lident name -> Lident (name ^ str_suffix)
    | Ldot (t, string) -> Ldot (name_mangle' t, string)
    | Lapply _ -> assert(false)
  in
  {lidentloc with txt = name_mangle' lidentloc.txt}

let str_split str =
  let pos = String.index str '#' in
  (String.sub str 0 pos, String.sub str pos (String.length str - pos))

let access_table str i =        (* str as orig_name *)
  try
    let open Dupfun in
    let dupfun = List.find (fun {orig_name; stamp} ->
        str = orig_name && i = stamp) !table in
    Some dupfun
  with _ -> None

let get_context name stamp =
  let open Dupfun in
  let get_dupfun orig_name i =
    List.find (fun {orig_name; stamp} ->
      orig_name = orig_name && i = stamp) !table in
  let get_context' dupfun str_suffix =
    List.find (fun suffix_cell ->
      str_suffix = str_of_suffix suffix_cell.suffix) dupfun.suffixes
    |> (fun x -> x.context)
  in
  try (* if the taget func don't have suffix, raise exp *)
    let (orig_name, str_suffix) = str_split name in
    let dupfun =  get_dupfun name stamp in
    Some (get_context' dupfun str_suffix)
  with _ -> None

let rec unalias_type env ty = (* Env.t -> type_expr -> type_expr *)
  match ty.desc with
  | Tconstr (path, types, abbrev_memo_ref) -> begin
      try
        Ctype.nondep_type env (Path.head path) ty
      with Not_found -> ty
    end
  | Tvar _ -> ty
  | Tarrow (l, ty1, ty2, com) ->
    {ty with Types.desc = Tarrow (l, unalias_type env ty1, unalias_type env ty2, com)}
  | Ttuple tyexprs ->
    {ty with Types.desc = Ttuple (List.map (unalias_type env) tyexprs)}
  | Tfield (name, field_kind, ty1, ty2) ->
    {ty with Types.desc = Tfield (name, field_kind, unalias_type env ty1, unalias_type env ty2)}
  | Tlink ty -> unalias_type env ty
  | _ -> ty

let sort gtyvars unif_result =
  List.map (fun id -> try
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
      if tylist != [] then P
      else begin match path with
      | Pident ident -> ident_name ident.name
      | _ -> P (* XXX: TODO: fix me *)
      end
    | Tobject _ -> P
    | Tfield _ -> P
    | Tlink ty -> inference ty
    | Tvar _ -> begin
      try
        List.assoc ty.id context
      with Not_found -> begin
          str_of_type ty |> Format.printf "name_inference: %s@.";
          Format.printf "context: ";
          List.iter (fun (i, l) ->
            Format.printf "%d %s, " i (str_of_literal l)) context;
          Format.printf "@.%d@." id;
          (* assert(false) *)
          I
        end
    end
    | _ -> assert(false) in
  match optty with
  | None -> I
  | Some ty -> inference ty

let inference env gtyvars context gty ty = (* gty = more general type *)
  (* int list -> type_expr -> type_expr -> suffix *)
  let ty1 = unalias_type env gty in
  let ty2 = unalias_type env ty in
  let unif_result = Unify.unify_typexpr ty1 ty2
    |> sort gtyvars in (* (int * opt type) list *)
  let suffix = List.map (name_inference context) unif_result in
  suffix

(* let enter_expression exp = *)
(*   let open Ident in *)
(*   match exp.exp_desc with *)
(*   | Texp_ident (path, lidentloc, vdesc) -> begin *)
(*       let {name; stamp} = Path.head path in *)
(*       Format.printf "%s@." name; *)
(*       match access_table name stamp with *)
(*       | None -> exp *)
(*       | Some dupfun -> *)
(*         let general_type = dupfun.Dupfun.ty in *)
(*         let gtyvars = dupfun.Dupfun.gtyvars in *)
(*         let context = if Stack.is_empty stack then [] else Stack.top stack in *)
(*         let env = exp.exp_env |> Envaux.env_of_only_summary in *)
(*         let suffix = inference env gtyvars context general_type exp.exp_type in *)
(*         let lidentloc = name_mangle lidentloc suffix in *)
(*         {exp with exp_desc = Texp_ident (path, lidentloc, vdesc)} *)
(*     end *)
(*   | Texp_let (_, vbs, _) -> *)
(*     let (names, stamps, types) = *)
(*       Dupfun.G.name_stamp_type vbs in *)
(*     List.map2 (fun name stamp -> get_context name stamp) *)
(*       names stamps *)
(*     |> List.map (function Some e -> e | None -> []) *)
(*     |> List.concat *)
(*     |> (@) (if Stack.is_empty stack then [] else Stack.top stack) *)
(*     |> (fun e -> Format.printf "now stack@."; e) *)
(*     |> (fun l -> List.iter (fun (i, l) -> *)
(*         Format.printf "%d, %s" i (Dupfun.str_of_literal l)) l; l) *)
(*     |> (fun e -> Format.printf "stack end@."; e) *)
(*     |> (fun e -> Stack.push e stack); *)
(*     Format.printf "(in:"; *)
(*     List.iter (Format.printf "%s ") names; *)
(*     Format.printf "@."; *)
(*     exp *)
(*   | _ -> exp *)

(* let leave_expression exp = *)
(*   match exp.exp_desc with *)
(*   | Texp_let _ -> *)
(*     Format.printf "out)@."; *)
(*     Stack.pop stack |> ignore; *)
(*     exp *)
(*   | _ -> exp *)

(* let enter_structure_item sitem = *)
(*   match sitem.str_desc with *)
(*   | Tstr_value (_, vbs) -> *)
(*     let (names, stamps, types) = *)
(*       Dupfun.G.name_stamp_type vbs in *)
(*     List.map2 (fun name stamp -> get_context name stamp) *)
(*       names stamps *)
(*     |> List.map (function Some e -> e | None -> []) *)
(*     |> List.concat *)
(*     |> (@) (if Stack.is_empty stack then [] else Stack.top stack) *)
(*     |> (fun e -> Stack.push e stack); *)
(*     Format.printf "(in:"; *)
(*     List.iter (Format.printf "%s ") names; *)
(*     Format.printf "@."; *)
(*     sitem *)
(*   | _ -> sitem *)

(* let leave_structure_item sitem = *)
(*   match sitem.str_desc with *)
(*   | Tstr_value _ -> *)
(*     Format.printf "out)@."; *)
(*     sitem *)
(*   | _ -> sitem *)

let rec value_binding context vb =
  (* context -> vb -> vb * context *)
  let make_new_context name stamp =
    match get_context name stamp with
    | None -> []
    | Some c -> c @ context
  in
  let names, stamps, _ = Dupfun.G.name_stamp_type [vb] in
  let c = List.map2 make_new_context names stamps
          |> List.concat in
  let exp = expression (c @ context) vb.vb_expr in
  let new_vb = {vb with vb_expr = exp} in
  (new_vb, c @ context)

and expression context exp =
  let desc =
    match exp.exp_desc with
    | Texp_ident (path, lidentloc, vdesc) as self-> begin
        try
          let {Ident.name; stamp} = Path.head path in
          match access_table name stamp with
          | None -> self
          | Some dupfun ->
            let general_type = dupfun.Dupfun.ty in
            let gtyvars = dupfun.Dupfun.gtyvars in
            let env = exp.exp_env |> Envaux.env_of_only_summary in
            let suffix = inference env gtyvars context general_type exp.exp_type in
            let lidentloc = name_mangle lidentloc suffix in
            Texp_ident (path, lidentloc, vdesc)
        with Fail_to_unify -> Texp_ident (path, lidentloc, vdesc)
      end
    | Texp_let (rec_flag, vbs, exp) ->
      let new_vbs, new_contexts = List.map (value_binding context) vbs
                                |> List.split in
      let new_context = List.flatten new_contexts in
      let new_exp = expression new_context exp in
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

and map_case context c =
  let c_guard = (function
      | None -> None
      | Some exp -> Some (expression context exp)) c.c_guard in
  let c_rhs = expression context c.c_rhs in
  {c with c_guard; c_rhs}

let rec structure_item str_item =
  let desc =
    match str_item.str_desc with
    | Tstr_value (rec_flag, vbs) ->
        let new_vbs, _ = List.map (value_binding []) vbs |> List.split in
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
      let mod_desc = Tmod_functor (ident, strloc, modtyopt, new_mod_expr) in
      {mod_expr with mod_desc}
  | _ -> mod_expr

and structure str =
  let items = List.map structure_item str.str_items in
  {str with str_items = items}

let structure str =
  table := !Dupfun.dupfun_table;
  structure str
