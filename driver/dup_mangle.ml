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

module Dmap =
  Map.Make(struct type t = int * string let compare = compare end)

let dmap = ref Dmap.empty
let update_dset k v  =
  dset := Dmap.add k v !dset

let ret_suffix name stamp =
  Dmap.find (name, stamp) !dset

module Context =
  Map.Make(struct type t = int let compare = compare end)

let merge_context a b =
  Context.merge (fun k x y ->
      match x, y with
      | None, None -> None
      | Some e, _ -> Some e
      | None, Some e -> Some e) a b

let value_binding vb ftvs =
  let gtvs = Ctype.free_variables vb.vb_pat.pat_type in
  if gtvs > 0 &&  then


let rec structure_item str_item =
  let desc =
    match str_item.str_desc with
    | Tstr_value (rec_flag, vbs) ->
        let new_vbs = value_binding vbs in
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
