(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Pierre Weis && Damien Doligez, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Warning definitions

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

type loc = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

type _ name =
  | Comment_start: unit name                (*  1 *)
  | Comment_not_end: unit name              (*  2 *)
(*| Deprecated --> alert "deprecated" *)    (*  3 *)
  | Fragile_match: string name              (*  4 *)
  | Partial_application: unit name          (*  5 *)
  | Labels_omitted: string list name        (*  6 *)
  | Method_override: string list name       (*  7 *)
  | Partial_match: string name              (*  8 *)
  | Non_closed_record_pattern: string name  (*  9 *)
  | Statement_type: unit name               (* 10 *)
  | Unused_match: unit name                 (* 11 *)
  | Unused_pat: unit name                   (* 12 *)
  | Instance_variable_override: string list name (* 13 *)
  | Illegal_backslash: unit name            (* 14 *)
  | Implicit_public_methods: string list name (* 15 *)
  | Unerasable_optional_argument: unit name (* 16 *)
  | Undeclared_virtual_method: string name  (* 17 *)
  | Not_principal: string name              (* 18 *)
  | Without_principality: string name       (* 19 *)
  | Unused_argument: unit name              (* 20 *)
  | Nonreturning_statement: unit name       (* 21 *)
  | Preprocessor: string name               (* 22 *)
  | Useless_record_with: unit name          (* 23 *)
  | Bad_module_name: string name            (* 24 *)
  | All_clauses_guarded: unit name          (* 8, used to be 25 *)
  | Unused_var: string name                 (* 26 *)
  | Unused_var_strict: string name          (* 27 *)
  | Wildcard_arg_to_constant_constr: unit name (* 28 *)
  | Eol_in_string: unit name                (* 29 *)
  | Duplicate_definitions: (string * string * string * string) name (* 30 *)
  | Multiple_definition: (string * string * string) name (* 31 *)
  | Unused_value_declaration: string name   (* 32 *)
  | Unused_open: string name                (* 33 *)
  | Unused_type_declaration: string name    (* 34 *)
  | Unused_for_index: string name           (* 35 *)
  | Unused_ancestor: string name            (* 36 *)
  | Unused_constructor: (string * bool * bool) name (* 37 *)
  | Unused_extension: (string * bool * bool * bool) name (* 38 *)
  | Unused_rec_flag: unit name              (* 39 *)
  | Name_out_of_scope: (string * string list * bool) name (* 40 *)
  | Ambiguous_name: (string list * string list *  bool * string) name (* 41 *)
  | Disambiguated_name: string name         (* 42 *)
  | Nonoptional_label: string name          (* 43 *)
  | Open_shadow_identifier: (string * string) name (* 44 *)
  | Open_shadow_label_constructor: (string * string) name (* 45 *)
  | Bad_env_variable: (string * string) name (* 46 *)
  | Attribute_payload: (string * string) name (* 47 *)
  | Eliminated_optional_arguments: string list name (* 48 *)
  | No_cmi_file: (string * string option) name (* 49 *)
  | Bad_docstring: bool name                (* 50 *)
  | Expect_tailcall: unit name              (* 51 *)
  | Fragile_literal_pattern: unit name      (* 52 *)
  | Misplaced_attribute: string name        (* 53 *)
  | Duplicated_attribute: string name       (* 54 *)
  | Inlining_impossible: string name        (* 55 *)
  | Unreachable_case: unit name             (* 56 *)
  | Ambiguous_pattern: string list name     (* 57 *)
  | No_cmx_file: string name                (* 58 *)
  | Assignment_to_non_mutable_value: unit name (* 59 *)
  | Unused_module: string name              (* 60 *)
  | Unboxable_type_in_prim_decl: string name (* 61 *)
  | Constraint_on_gadt: unit name           (* 62 *)
  | Erroneous_printed_signature: string name (* 63 *)
  | Unsafe_without_parsing: unit name       (* 64 *)
  | Redefining_unit: string name            (* 65 *)
  | Unused_open_bang: string name           (* 66 *)
  | Unused_functor_parameter: string name   (* 67 *)
;;

type t

val mk : 'a name -> 'a -> t

type alert = {kind:string; message:string; def:loc; use:loc}

val parse_options : bool -> string -> unit;;

val parse_alert_option: string -> unit
  (** Disable/enable alerts based on the parameter to the -alert
      command-line option.  Raises [Arg.Bad] if the string is not a
      valid specification.
  *)

val without_warnings : (unit -> 'a) -> 'a
  (** Run the thunk with all warnings and alerts disabled. *)

val is_active : _ name -> bool;;
val is_error : _ name -> bool;;

val defaults_w : string;;
val defaults_warn_error : string;;

type reporting_information =
  { id : string
  ; message : string
  ; is_error : bool
  ; sub_locs : (loc * string) list;
  }

val report : t -> [ `Active of reporting_information | `Inactive ]
val report_alert : alert -> [ `Active of reporting_information | `Inactive ]

exception Errors;;

val check_fatal : unit -> unit;;
val reset_fatal: unit -> unit

val help_warnings: unit -> unit

type state
val backup: unit -> state
val restore: state -> unit
val mk_lazy: (unit -> 'a) -> 'a Lazy.t
    (** Like [Lazy.of_fun], but the function is applied with
        the warning/alert settings at the time [mk_lazy] is called. *)
