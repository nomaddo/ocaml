(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* The batch compiler *)

open Misc
open Config
open Format
open Typedtree
open Compenv

(* [begin tokuda] copipe from flag.ml *)
module Dup_debug_flag = struct
  let moded_t = false             (* affect compile.ml and optcompile.ml *)
  let fun_trace = false           (* affect mod.ml *)
  let var_trace = false           (* affect mod.ml *)
  let too_big = true              (* affect mod.ml *)
  let dup_fun_table = false       (* affect mod.ml *)
  let stage_debug = false         (* affect compile and optcompile *)
  let unification_result = false  (* affect dmod.ml *)
  let dmod_stack_flag = false      (* affect dmod.ml *)
  let dmod_dup_fun_table = false   (* affect dmod.ml *)
  let inference = false           (* affect dmod.ml *)
end
(* [end tokuda] *)

(* Compile a .mli file *)

(* Keep in sync with the copy in compile.ml *)

let tool_name = "ocamlopt"

let interface ppf sourcefile outputprefix =
  Compmisc.init_path false;
  let modulename = module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name modulename;
  let initial_env = Compmisc.initial_env () in
  let ast = Pparse.parse_interface ~tool_name ppf sourcefile in
  if !Clflags.dump_parsetree then fprintf ppf "%a@." Printast.interface ast;
  if !Clflags.dump_source then fprintf ppf "%a@." Pprintast.signature ast;
  let tsg = Typemod.type_interface initial_env ast in
  if !Clflags.dump_typedtree then fprintf ppf "%a@." Printtyped.interface tsg;
  let sg = tsg.sig_type in
  if !Clflags.print_types then
    Printtyp.wrap_printing_env initial_env (fun () ->
        fprintf std_formatter "%a@."
          Printtyp.signature (Typemod.simplify_signature sg));
  ignore (Includemod.signatures initial_env sg sg);
  Typecore.force_delayed_checks ();
  Warnings.check_fatal ();
  if not !Clflags.print_types then begin
    let sg = Env.save_signature sg modulename (outputprefix ^ ".cmi") in
    Typemod.save_signature modulename tsg outputprefix sourcefile
      initial_env sg ;
  end

(* Compile a .ml file *)

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x
let (+++) (x, y) f = (x, f y)

let implementation ppf sourcefile outputprefix =
  Compmisc.init_path true;
  (* Clflags.mydump := false; *)
  let modulename = module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name modulename;
  let env = Compmisc.initial_env() in
  Compilenv.reset ?packname:!Clflags.for_package modulename;
  let cmxfile = outputprefix ^ ".cmx" in
  let objfile = outputprefix ^ ext_obj in
  let comp ast =
    if !Clflags.print_types
    then
      ast
      ++ print_if ppf Clflags.dump_parsetree Printast.implementation
      ++ print_if ppf Clflags.dump_source Pprintast.structure
      ++ Typemod.type_implementation sourcefile outputprefix modulename env
      ++ print_if ppf Clflags.dump_typedtree
          Printtyped.implementation_with_coercion
      ++ (fun _ -> ())
    else begin
      ast
      ++ print_if ppf Clflags.dump_parsetree Printast.implementation
      ++ print_if ppf Clflags.dump_source Pprintast.structure
      (* [begin tokuda] *)
      ++ (fun ptree ->
          if !Clflags.mydump then
            Format.eprintf "[begin tokuda]\n%a\n[end tokuda]@."
              Pprintast.structure ptree;
          ptree)
      (* [end tokuda] *)
      ++ Typemod.type_implementation sourcefile outputprefix modulename env

      (* [begin tokuda] do the duplication *)
      ++ (fun (typedtree, module_corcion) ->
        Dupfun.structure typedtree
        |> Rename_ident.structure, module_corcion)
      (* [end tokuda] *)
      ++ (fun x ->
          (if Dup_debug_flag.stage_debug
           then Format.eprintf "DEBUG: before Untypeast.untype_structure@.");
          x)
      ++ (fun (str, module_coercion) -> (* for Dup_debug_flag.stage_debug *)
          let ptree = Untypeast.untype_structure str in
          if !Clflags.mydump then
            Format.eprintf "[begin tokuda]\n%a\n[end tokuda]@."
              Pprintast.structure ptree;
          ptree)
      ++ (fun x ->
          (if Dup_debug_flag.stage_debug
           then Format.eprintf "DEBUG: end Untypeast.untype_structure@.");
          x)
      (* 2nd typing *)
      ++ (fun x ->
          (if Dup_debug_flag.stage_debug
           then Format.eprintf "DEBUG: before 2nd typing@.");
          x)
      ++ (fun ptree ->
          (* [XXX] which of these initialiations are necessary? *)
          Compmisc.init_path false;
          Env.set_unit_name modulename;
          let env = Compmisc.initial_env () in
          Compilenv.reset ?packname:!Clflags.for_package modulename;
          Typemod.type_implementation sourcefile outputprefix modulename env ptree
      )
      ++ (fun x ->
          (if Dup_debug_flag.stage_debug
           then Format.eprintf "DEBUG: after 2nd typing@.");
          x)
      (* 2nd untyping *)
      ++ (fun (str, module_coercion) -> (* for debug *)
          let ptree = Untypeast.untype_structure str in
          (if !Clflags.mydump
           then Format.eprintf "%a@." Pprintast.structure ptree);
          str, module_coercion)
      (* [end tokuda] *)
      ++ print_if ppf Clflags.dump_typedtree
          Printtyped.implementation_with_coercion
      ++ Translmod.transl_store_implementation modulename
      +++ print_if ppf Clflags.dump_rawlambda Printlambda.lambda
      +++ Simplif.simplify_lambda
      +++ print_if ppf Clflags.dump_lambda Printlambda.lambda
      ++ Asmgen.compile_implementation outputprefix ppf;
      Compilenv.save_unit_info cmxfile;
    end;
    Warnings.check_fatal ();
    Stypes.dump (Some (outputprefix ^ ".annot"))
  in
  try comp (Pparse.parse_implementation ~tool_name ppf sourcefile)
  with x ->
    Stypes.dump (Some (outputprefix ^ ".annot"));
    remove_file objfile;
    remove_file cmxfile;
    raise x

let c_file name =
  if Ccomp.compile_file name <> 0 then exit 2
