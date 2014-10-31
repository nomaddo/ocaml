open Typedtree
open Dupfun
open Asttypes

let str_split = Rename_ident.str_split

let sig_intf = ref (Obj.magic ())
let exists = ref false
let set_sig optintf =
  match optintf with
  | None -> ()
  | Some intf -> sig_intf := intf

let is_public id =
  let rec search = function
    | [] -> false
    | x::xs -> begin match x with
        | Types.Sig_value (sid, vdesc) ->
            if Ident.same id sid then true else search xs
        | _ -> search xs
      end in
  if !exists then search !sig_intf else true

let value_bindings vbs =
  let rec loop = function
    | vb :: xs -> begin
        let attr = vb.vb_attributes in
        try
          let cells : Dupfun.suffix_cell ref list =
            List.filter (fun (strloc, _) -> strloc.txt = Dupfun.dup_label) attr
            |> List.map (fun (strloc, _) -> strloc.loc) |> Obj.magic in
          let include_public =
            let ids = Typedtree.pat_bound_idents vb.vb_pat
                      |> List.map fst in
            List.map is_public ids |> List.exists ((=) true) in
          if List.exists (fun c -> (!c).is_used) cells || include_public
          then vb :: loop xs
          else loop xs
        with Not_found -> vb :: loop xs
      end
    | [] -> [] in
  loop vbs

let rec structure_item str_item =
  let desc =
    match str_item.str_desc with
    | Tstr_value (r, vbs) ->
        let vbs = value_bindings vbs in
        Tstr_value (r, vbs)
    | Tstr_module mb ->
        let mb_expr = module_expr mb.mb_expr in
        Tstr_module {mb with mb_expr = mb_expr}
    | _ as self -> self in
  {str_item with str_desc = desc}

and module_expr mod_expr =
  match mod_expr.mod_desc with
  | Tmod_structure str ->
      let new_str = structure str in
      {mod_expr with mod_desc = Tmod_structure new_str}
  | _ -> mod_expr

and structure str =
  let items = List.map structure_item str.str_items in
  {str with str_items = items}


let structure _sig str =
  set_sig _sig;
  if !Clflags.tbl then
    List.iter Dupfun.print_dupfun !Dupfun.dupfun_table;
  structure str
