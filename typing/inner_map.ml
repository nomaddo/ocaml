type tvar_map = (int, int) Hashtbl.t
type map_tbl = (Path.t, tvar_map) Hashtbl.t

let map_tbl : map_tbl = Hashtbl.create 100
let current_tbl = ref (None, Hashtbl.create 100)

type order = Pos_to_neg | Neg_to_pos

let switch = ref Pos_to_neg

(* for debug *)
let tvar_map fmt (h: tvar_map) =
  Hashtbl.iter (fun k v -> Format.fprintf fmt "(%d %d) " k v) h;
  print_endline ""

let print_map_tbl fmt (t: map_tbl) =
  Hashtbl.iter (fun k v -> Format.fprintf fmt "%a: %a@." Path.print k tvar_map v) t

let begin_create_tbl path =
  try
    let h = Hashtbl.find map_tbl path in
    current_tbl := (Some path, h)
  with Not_found ->
    let new_tbl = Hashtbl.create 1000 in
    Hashtbl.add map_tbl path new_tbl;
    current_tbl := (Some path, new_tbl)

let begin_cmi_export () =
  current_tbl := (None, Hashtbl.create 100)

let create_alias p1 p2 =
  let h = Hashtbl.find map_tbl p1 in
  Hashtbl.add map_tbl p2 h

let add id1 id2 =
  let tbl = snd !current_tbl in
  match !switch with
  | Pos_to_neg ->
      if id1 > id2
      then Hashtbl.add tbl id1 id2
      else Hashtbl.add tbl id2 id1
  | Neg_to_pos ->
      if id1 > id2
      then Hashtbl.add tbl id2 id1
      else Hashtbl.add tbl id1 id2

let get_map path = Hashtbl.find map_tbl path

let cmi_tbl : tvar_map option ref = ref None
let begin_coercion () =
  cmi_tbl := Some (Hashtbl.create 10)

let add_cmi_tbl id1 id2 =
  match !cmi_tbl with
  | Some h -> Hashtbl.add h id2 id1
  | None -> assert false

let reset () =
  Hashtbl.clear map_tbl;
  current_tbl := (None, Hashtbl.create 100);
  switch := Pos_to_neg;
  cmi_tbl := None
