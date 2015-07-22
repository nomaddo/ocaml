type tvar_map = (int, int) Hashtbl.t
type map_tbl = (Path.t, tvar_map) Hashtbl.t

let map_tbl : map_tbl = Hashtbl.create 100
let current_tbl = ref (Hashtbl.create 100)

let print_path = ref (fun _ -> failwith "not implemented")

(* for debug *)
let tvar_map fmt (h: tvar_map) =
  Hashtbl.iter (fun k v -> Format.fprintf fmt "(%d %d) " k v) h;
  print_endline ""

let print_map_tbl fmt (t: map_tbl) =
  Hashtbl.iter (fun k v -> Format.fprintf fmt "%a:@.%a@." !print_path k tvar_map v) t

let begin_create_tbl path =
  try
    let h = Hashtbl.find map_tbl path in
    current_tbl := h
  with Not_found ->
    let new_tbl = Hashtbl.create 1000 in
    Hashtbl.add map_tbl path new_tbl;
    current_tbl := new_tbl

let begin_cmi_export () =
  current_tbl := Hashtbl.create 100

let create_alias p1 p2 =
  let h = Hashtbl.find map_tbl p1 in
  Hashtbl.add map_tbl p2 h

let add id1 id2 = Hashtbl.add !current_tbl id1 id2

let get_map = function
  | Path.Pident _ -> None
  | Path.Papply _ -> raise Exit
  | Path.Pdot (path, _, _) -> match Hashtbl.find_all map_tbl path with
    | [x] -> Some x
    | [] -> Format.printf "get_map: not_found: %a@." print_map_tbl map_tbl; assert false
    | _ -> Format.printf "get_map: invalid status: %a@." print_map_tbl map_tbl; assert false
