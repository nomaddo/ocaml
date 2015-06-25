open Types

(* Tbl for remebering which function is polymorphic *)
let tbl : (Ident.t, (type_expr * type_expr list) option) Hashtbl.t =
  Hashtbl.create 100
