(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Generate an .annot file from a .cmt file. *)

open Asttypes
open Typedtree
open Tast_mapper

let bind_variables scope =
  let super = Tast_mapper.default in
  let pat sub p =
    begin match p.pat_desc with
    | Tpat_var (id, _) | Tpat_alias (_, id, _) ->
        Stypes.record (Stypes.An_ident (p.pat_loc,
                                        Ident.name id,
                                        Annot.Idef scope))
    | _ -> ()
    end;
    super.pat sub p
  in
  {super with pat}

let bind_variables scope =
  let o = bind_variables scope in
  fun p -> ignore (o.pat o p)

let bind_bindings scope bindings =
  let o = bind_variables scope in
  List.iter (fun x -> o x.vb_pat) bindings

let bind_cases l =
  List.iter
    (fun {c_lhs; c_guard; c_rhs} ->
      let loc =
        let open Location in
        match c_guard with
        | None -> c_rhs.exp_loc
        | Some g -> {c_rhs.exp_loc with loc_start=g.exp_loc.loc_start}
      in
      bind_variables loc  c_lhs
    )
    l

let record_module_binding scope mb =
  Stypes.record (Stypes.An_ident
                   (mb.mb_name.loc,
                    mb.mb_name.txt,
                    Annot.Idef scope))

let pos_is_dummy = function
  | {Lexing.pos_lnum = 0; pos_bol = 0; pos_cnum = -1; _} -> true
  | _ -> false

let rec iterator ~scope rebuild_env =
  let super = Tast_mapper.default in
  let class_expr sub node =
    Stypes.record (Stypes.Ti_class node);
    super.class_expr sub node

  and module_expr _sub node =
    Stypes.record (Stypes.Ti_mod node);
    super.module_expr (iterator ~scope:node.mod_loc rebuild_env) node

  and expr sub exp =
    begin match exp.exp_desc with
    | Texp_ident (path, _, _) ->
        let full_name = Path.name ~paren:Oprint.parenthesized_ident path in
        let env =
          if rebuild_env then
            Env.env_of_only_summary Envaux.env_from_summary exp.exp_env
          else
            exp.exp_env
        in
        let annot =
          try
            let desc = Env.find_value path env in
            let dloc = desc.Types.val_loc in
            if pos_is_dummy dloc.Location.loc_start ||
               pos_is_dummy dloc.Location.loc_end then Annot.Iref_external
            else Annot.Iref_internal dloc
          with Not_found ->
            Annot.Iref_external
        in
        Stypes.record
          (Stypes.An_ident (exp.exp_loc, full_name , annot))
    | Texp_let (Recursive, bindings, _) ->
        bind_bindings exp.exp_loc bindings
    | Texp_let (Nonrecursive, bindings, body) ->
        bind_bindings body.exp_loc bindings
    | Texp_match (_, f1, _) ->
        bind_cases f1
    | Texp_function { cases = f; }
    | Texp_try (_, f) ->
        bind_cases f
    | Texp_letmodule (_, modname, _, body ) ->
        Stypes.record (Stypes.An_ident
                       (modname.loc,modname.txt,Annot.Idef body.exp_loc))
    | _ -> ()
    end;
    Stypes.record (Stypes.Ti_expr exp);
    super.expr sub exp

  and pat sub p =
    Stypes.record (Stypes.Ti_pat p);
    super.pat sub p
  in

  let structure_item_rem sub str rem =
    let open Location in
    let loc = str.str_loc in
    begin match str.str_desc with
    | Tstr_value (rec_flag, bindings) ->
        let doit loc_start = bind_bindings {scope with loc_start} bindings in
        begin match rec_flag, rem with
        | Recursive, _ -> doit loc.loc_start
        | Nonrecursive, [] -> doit loc.loc_end
        | Nonrecursive,  {str_loc = loc2} :: _ -> doit loc2.loc_start
        end
    | Tstr_module mb ->
        record_module_binding
          { scope with Location.loc_start = loc.loc_end } mb
    | Tstr_recmodule mbs ->
        List.iter (record_module_binding
                   { scope with Location.loc_start = loc.loc_start }) mbs
    | _ ->
        ()
    end;
    Stypes.record_phrase loc;
    super.structure_item sub str
  in
  let structure_item sub s =
    (* This will be used for Partial_structure_item.
       We don't have here the location of the "next" item,
       this will give a slightly different scope for the non-recursive
       binding case. *)
    structure_item_rem sub s []
  and structure sub l =
    let rec loop = function
      | str :: rem -> structure_item_rem sub str rem :: loop rem
      | [] -> []
    in
    {l with str_items = loop l.str_items}
  in
  {super with class_expr; module_expr; expr; pat; structure_item; structure}

let iterator ?sourcefile ~use_summaries =
  let scope =
    match sourcefile with
    | None -> Location.none
    | Some file -> Location.in_file file
  in
  iterator ~scope use_summaries
