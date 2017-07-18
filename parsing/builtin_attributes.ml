(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Parsetree

let string_of_cst = function
  | Pconst_string(s, _) -> Some s
  | _ -> None

let string_of_payload = function
  | PStr[{pstr_desc=Pstr_eval({pexp_desc=Pexp_constant c},_)}] ->
      string_of_cst c
  | _ -> None

let string_of_opt_payload p =
  match string_of_payload p with
  | Some s -> s
  | None -> ""

let rec error_of_extension ext =
  match ext with
  | ({txt = ("ocaml.error"|"error") as txt; loc}, p) ->
    let rec sub_from inner =
      match inner with
      | {pstr_desc=Pstr_extension (ext, _)} :: rest ->
          error_of_extension ext :: sub_from rest
      | _ :: rest ->
          (Location.errorf ~loc
             "Invalid syntax for sub-error of extension '%s'." txt) ::
            sub_from rest
      | [] -> []
    in
    begin match p with
    | PStr [] -> raise Location.Already_displayed_error
    | PStr({pstr_desc=Pstr_eval
              ({pexp_desc=Pexp_constant(Pconst_string(msg,_))}, _)}::
           {pstr_desc=Pstr_eval
              ({pexp_desc=Pexp_constant(Pconst_string(if_highlight,_))}, _)}::
           inner) ->
        Location.error ~loc ~if_highlight ~sub:(sub_from inner) msg
    | PStr({pstr_desc=Pstr_eval
              ({pexp_desc=Pexp_constant(Pconst_string(msg,_))}, _)}::inner) ->
        Location.error ~loc ~sub:(sub_from inner) msg
    | _ -> Location.errorf ~loc "Invalid syntax for extension '%s'." txt
    end
  | ({txt; loc}, _) ->
      Location.errorf ~loc "Uninterpreted extension '%s'." txt

let cat s1 s2 =
  if s2 = "" then s1 else s1 ^ "\n" ^ s2

let rec deprecated_of_attrs = function
  | [] -> None
  | ({txt = "ocaml.deprecated"|"deprecated"; _}, p) :: _ ->
      Some (string_of_opt_payload p)
  | _ :: tl -> deprecated_of_attrs tl

let check_deprecated loc warns attrs s =
  match deprecated_of_attrs attrs with
  | None -> ()
  | Some txt -> Location.deprecated loc warns (cat s txt)

let check_deprecated_inclusion ~def ~use loc warns attrs1 attrs2 s =
  match deprecated_of_attrs attrs1, deprecated_of_attrs attrs2 with
  | None, _ | Some _, Some _ -> ()
  | Some txt, None -> Location.deprecated ~def ~use loc warns (cat s txt)

let rec deprecated_mutable_of_attrs = function
  | [] -> None
  | ({txt = "ocaml.deprecated_mutable"|"deprecated_mutable"; _}, p) :: _ ->
      Some (string_of_opt_payload p)
  | _ :: tl -> deprecated_mutable_of_attrs tl

let check_deprecated_mutable loc warns attrs s =
  match deprecated_mutable_of_attrs attrs with
  | None -> ()
  | Some txt ->
      Location.deprecated loc warns (Printf.sprintf "mutating field %s" (cat s txt))

let check_deprecated_mutable_inclusion ~def ~use loc warns attrs1 attrs2 s =
  match deprecated_mutable_of_attrs attrs1,
        deprecated_mutable_of_attrs attrs2
  with
  | None, _ | Some _, Some _ -> ()
  | Some txt, None ->
      Location.deprecated ~def ~use loc warns
        (Printf.sprintf "mutating field %s" (cat s txt))

let rec deprecated_of_sig = function
  | {psig_desc = Psig_attribute a} :: tl ->
      begin match deprecated_of_attrs [a] with
      | None -> deprecated_of_sig tl
      | Some _ as r -> r
      end
  | _ -> None


let rec deprecated_of_str = function
  | {pstr_desc = Pstr_attribute a} :: tl ->
      begin match deprecated_of_attrs [a] with
      | None -> deprecated_of_str tl
      | Some _ as r -> r
      end
  | _ -> None


let emit_external_warnings warns =
  (* Note: this is run as a preliminary pass when type-checking an
     interface or implementation.  This allows to cover all kinds of
     attributes, but the drawback is that it doesn't take local
     configuration of warnings (with '@@warning'/'@@warnerror'
     attributes) into account.  We should rather check for
     'ppwarning' attributes during the actual type-checking, making
     sure to cover all contexts (easier and more ugly alternative:
     duplicate here the logic which control warnings locally). *)
  let open Ast_iterator in
  {
    default_iterator with
    attribute = (fun _ a ->
        match a with
        | {txt="ocaml.ppwarning"|"ppwarning"},
          PStr[{pstr_desc=Pstr_eval({pexp_desc=Pexp_constant
                                         (Pconst_string (s, _))},_);
                pstr_loc}] ->
            Location.prerr_warning pstr_loc warns (Warnings.Preprocessor s)
        | _ -> ()
      )
  }


let warning_attribute warns attrs =
  let process loc warns txt errflag payload =
    match string_of_payload payload with
    | Some s ->
        begin try Warnings.parse_options errflag warns s
        with Arg.Bad _ ->
          Location.prerr_warning loc warns
            (Warnings.Attribute_payload
               (txt, "Ill-formed list of warnings"));
          warns
        end
    | None ->
        Location.prerr_warning loc warns
          (Warnings.Attribute_payload
             (txt, "A single string literal is expected"));
        warns
  in
  List.fold_left
    (fun warns attr ->
       match attr with
       | ({txt = ("ocaml.warning"|"warning") as txt; loc}, payload) ->
           process loc warns txt false payload
       | ({txt = ("ocaml.warnerror"|"warnerror") as txt; loc}, payload) ->
           process loc warns txt true payload
       | _ ->
           warns
    ) warns attrs

let warn_on_literal_pattern =
  List.exists
    (function
      | ({txt="ocaml.warn_on_literal_pattern"|"warn_on_literal_pattern"; _}, _)
        -> true
      | _ -> false
    )

let explicit_arity =
  List.exists
    (function
      | ({txt="ocaml.explicit_arity"|"explicit_arity"; _}, _) -> true
      | _ -> false
    )

let immediate =
  List.exists
    (function
      | ({txt="ocaml.immediate"|"immediate"; _}, _) -> true
      | _ -> false
    )

(* The "ocaml.boxed (default)" and "ocaml.unboxed (default)"
   attributes cannot be input by the user, they are added by the
   compiler when applying the default setting. This is done to record
   in the .cmi the default used by the compiler when compiling the
   source file because the default can change between compiler
   invocations. *)

let check l (x, _) = List.mem x.txt l

let has_unboxed attr =
  List.exists (check ["ocaml.unboxed"; "unboxed"])
    attr

let has_boxed attr =
  List.exists (check ["ocaml.boxed"; "boxed"]) attr
