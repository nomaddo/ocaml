type tvar_map = (int, int) Hashtbl.t
type map_tbl = (string, tvar_map) Hashtbl.t

let inter_tbl : tvar_map = Hashtbl.create 10

type phase = Typing | Transl | MakingCmi
let string_of_phase = function
  Typing -> "Typing" | Transl -> "Transl" | MakingCmi -> "MakingCmi"

let switch = ref Typing

(* for debug *)
let tvar_map fmt (h: tvar_map) =
  Hashtbl.iter (fun k v -> Format.fprintf fmt "(%d %d) " k v) h

let print_map_tbl fmt (t: map_tbl) =
  Hashtbl.iter (fun k v -> Format.fprintf fmt "%s: %a@."
                   k tvar_map v) t

let cmi_tbl : tvar_map = Hashtbl.create 10

(* for cutting each phases *)
let begin_cmi_export () =
  switch := MakingCmi

let add old new_ =
  (* begin if !Clflags.dump_addvar then *)
  (*   Format.printf "%s: %d %d@." *)
  (*     (string_of_phase !switch) old new_ end; *)
  match !switch with
  | Typing ->
      Hashtbl.add inter_tbl new_ old
  | Transl -> ()
  | MakingCmi ->
      Hashtbl.add cmi_tbl old new_

let add_to_cmi id1 id2 =
  Hashtbl.add cmi_tbl id1 id2

let reset () =
  Hashtbl.reset inter_tbl;
  switch := Typing;
  Hashtbl.reset cmi_tbl

let () =
  Btype.add_tbl := add
