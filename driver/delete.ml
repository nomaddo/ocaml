open Typedtree
open Dupfun
open Asttypes
open Ident

let dupfun_table = Dupfun.dupfun_table

module Defined = struct
  module D = Set.Make(struct type t = string let compare = compare end)
  include D

  let set = ref D.empty
  let update e = set := D.add e !set
end

let str_split = Rename_ident.str_split

let substring str sub =
  let len = String.length sub in
  try
    for i = 0 to len - 1 do
      if not (str.[i] = sub.[i]) then raise Exit
    done; true
  with Exit -> false
     | Invalid_argument _ -> false

let orig_name str =
  (fun (s, _) -> s) (str_split str)

let sig_intf = ref (Obj.magic ())
let exists = ref false
let set_sig optintf =
  match optintf with
  | None -> ()
  | Some intf -> exists := true; sig_intf := intf

let is_public id =
  let rec search = function
    | [] -> false
    | x::xs -> begin match x with
        | Types.Sig_value (sid, vdesc) ->
            if substring id.name sid.name then true else search xs
        | _ -> search xs
      end in
  let defined id = Defined.mem (orig_name id.name) !Defined.set in
  if !exists then
    if defined id then false else search !sig_intf
  else true

let value_bindings vbs =
  let rec loop acc = function
    | vb :: xs -> begin
        let attr = vb.vb_attributes in
        let cells : Dupfun.suffix_cell ref list =
          List.filter (fun (strloc, _) -> strloc.txt = Dupfun.dup_label) attr
          |> List.map (fun (strloc, _) -> strloc.loc) |> Obj.magic in
        let ids = Typedtree.pat_bound_idents vb.vb_pat
                  |> List.map fst in
        let include_public =
          List.map is_public ids |> List.exists ((=) true) in
        let names = List.fold_left (fun s id ->
            let orig, suf = str_split id.name in
            if suf = "" then orig::s else s) [] ids in
        if List.exists (fun c -> (!c).is_used) cells || include_public || cells = []
        then vb :: loop (names @ acc) xs
        else loop (names @ acc) xs
      end
    | [] -> List.iter Defined.update acc; [] in
  loop [] vbs

let rec structure_item str_item =
  let desc =
    match str_item.str_desc with
    | Tstr_value (r, vbs) ->
        let vbs = value_bindings vbs in
        assert(vbs <> []);
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
  let items = List.rev str.str_items
              |> List.map structure_item
              |> List.rev in
  {str with str_items = items}

let structure _sig str =
  set_sig _sig;
  if !Clflags.tbl then
    Hashtbl.iter (fun _ d -> print_dupfun d) dupfun_table;
  structure str
