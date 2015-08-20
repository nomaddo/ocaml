type tvar_map = (int, int) Hashtbl.t
type map_tbl = (Path.t, tvar_map) Hashtbl.t

let map_tbl : map_tbl ref = ref (Hashtbl.create 10)
let current_tbl = ref (None, Hashtbl.create 10)

type phase = Typing | Transl | MakingCmi
let string_of_phase = function
  Typing -> "Typing" | Transl -> "Transl" | MakingCmi -> "MakingCmi"

let switch = ref Typing

(* for debug *)
let tvar_map fmt (h: tvar_map) =
  Hashtbl.iter (fun k v -> Format.fprintf fmt "(%d %d) " k v) h

let print_map_tbl fmt (t: map_tbl) =
  Hashtbl.iter (fun k v -> Format.fprintf fmt "%a: %a@."
                   Path.print k tvar_map v) t

let add_tbl path =
  try
    let h = Hashtbl.find !map_tbl path in
    current_tbl := (Some path, h)
  with Not_found ->
    let new_tbl = Hashtbl.create 1 in
    Hashtbl.add !map_tbl path new_tbl;
    current_tbl := (Some path, new_tbl)

(* for cutting each phases *)
let begin_cmi_export () =
  switch := MakingCmi;
  current_tbl := (None, Hashtbl.create 10)

let add id1 id2 =
  (* Format.printf "%s: %a: %d %d@." (string_of_phase !switch) *)
  (*   (fun fmt opt -> match opt with None -> Format.fprintf fmt "None" *)
  (*                                | Some path -> Path.print fmt path) (fst !current_tbl) id1 id2; *)
  match !switch with
  | Typing ->
      let tbl = snd !current_tbl in
      Hashtbl.add tbl id2 id1
  | Transl -> ()
  | MakingCmi ->
      let opt, tbl = !current_tbl in
      assert (opt = None);
      Hashtbl.add tbl id1 id2

let get_map path = Hashtbl.find !map_tbl path

let cmi_tbl : tvar_map option ref = ref None
let begin_coercion () =
  cmi_tbl := Some (Hashtbl.create 10)

let add_to_cmi id1 id2 =
  match !cmi_tbl with
  | Some h -> Hashtbl.add h id2 id1
  | None -> assert false

let reset () =
  current_tbl := (None, Hashtbl.create 10);
  !map_tbl |> Hashtbl.reset;
  switch := Typing;
  cmi_tbl := None

let () =
  Btype.add_tbl := add
