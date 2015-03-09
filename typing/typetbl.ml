open Types
let tbl : (Ident.t, (type_expr * type_expr list) option) Hashtbl.t =
  Hashtbl.create 100

let is_gadt_tbl : (Path.t, bool ) Hashtbl.t =
  Hashtbl.create 100

let is_gadt : Env.t -> Path.t -> bool = fun env path ->
  match Hashtbl.find_all is_gadt_tbl path with
  | [] -> begin (*  first case *)
      let type_decl = Env.find_type path env in
      let ans = match type_decl.type_kind with
        | Type_abstract | Type_record _ | Type_open -> false
        | Type_variant cds -> begin try
              List.iter
                (fun {cd_res} -> match cd_res with None -> ()
                                             | Some _ -> raise Exit) cds
              |> ignore; false
            with Exit -> true end in
      Hashtbl.add is_gadt_tbl path ans; ans
    end
  | [ans] -> ans
  | _ -> assert false
