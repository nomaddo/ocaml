open TypedtreeMap
open Typedtree
open Types

module Argument = struct
  include DefaultMapArgument
  let table = !Dupfun.dupfun_table

  let access_table table str i = try
      Some (List.find (fun {orig_name; stamp} ->
          orig_name = str && i = stamp) table)
    with _ -> None

  let name_mangle lidentloc suffix =
    let str_suffix = List.map Dupfun.str_of_literal suffix in
    let rec name_mangle' lident =
      match lident with
      | Lident name -> Lident (name ^ str_suffix)
      | Ldot (t, string) -> Ldot (name_mangle' t, string)
      | Lapply _ -> assert(false)
    in
    {lidentloc with txt = name_mangle' lidentloc.txt}

  let inference gtyvars ty1 ty2 =
    (* int list -> type_expr -> type_expr -> (int * literal) list *)
    let


  let enter_expression exp =
    let open Ident in
    match exp.exp_desc with
    | Texp_ident (path, lidentloc, vdesc) ->
        let ident = Path.head path in
        let name = ident.name
        and stamp = ident.stamp in
        match access_table table name stamp with
        | None -> exp
        | Some dupfun ->
