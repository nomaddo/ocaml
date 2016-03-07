type tvar_map = (int, int) Hashtbl.t
type map_tbl = (string, tvar_map) Hashtbl.t

let inter_tbl : tvar_map = Hashtbl.create 10

type phase = Typing | MakingCmi
let string_of_phase = function
  Typing -> "Typing" | MakingCmi -> "MakingCmi"

let switch = ref Typing

type type_kind = I | F | P | Kvar of int | Gen

(* for debug *)
let tvar_map fmt (h: tvar_map) =
  Hashtbl.iter (fun k v -> Format.fprintf fmt "(%d %d) " k v) h

let print_map_tbl fmt (t: map_tbl) =
  Hashtbl.iter (fun k v -> Format.fprintf fmt "%s: %a@."
                   k tvar_map v) t

let cmi_tbl = Hashtbl.create 10

(* for cutting each phases *)
let begin_cmi_export () =
  switch := MakingCmi

let add_to_cmi id kind =
  Hashtbl.add cmi_tbl id kind

let add old new_ =
  match !switch with
  | Typing ->
      if not (old > 0) then Hashtbl.add inter_tbl new_ old
  | MakingCmi -> add_to_cmi old (Kvar new_)

let reset () =
  Hashtbl.reset inter_tbl;
  switch := Typing;
  Hashtbl.reset cmi_tbl

let () =
  Btype.add_tbl := add;
