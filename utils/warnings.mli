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

type _ t =
  | Comment_start: unit t                (*  1 *)
  | Comment_not_end: unit t              (*  2 *)
(*| Deprecated --> alert "deprecated" *) (*  3 *)
  | Fragile_match: string t              (*  4 *)
  | Partial_application: unit t          (*  5 *)
  | Labels_omitted: string list t        (*  6 *)
  | Method_override: string list t       (*  7 *)
  | Partial_match: string t              (*  8 *)
  | Non_closed_record_pattern: string t  (*  9 *)
  | Statement_type: unit t               (* 10 *)
  | Unused_match: unit t                 (* 11 *)
  | Unused_pat: unit t                   (* 12 *)
  | Instance_variable_override: string list t (* 13 *)
  | Illegal_backslash: unit t            (* 14 *)
  | Implicit_public_methods: string list t (* 15 *)
  | Unerasable_optional_argument: unit t (* 16 *)
  | Undeclared_virtual_method: string t  (* 17 *)
  | Not_principal: string t              (* 18 *)
  | Without_principality: string t       (* 19 *)
  | Unused_argument: unit t              (* 20 *)
  | Nonreturning_statement: unit t       (* 21 *)
  | Preprocessor: string t               (* 22 *)
  | Useless_record_with: unit t          (* 23 *)
  | Bad_module_name: string t            (* 24 *)
  | All_clauses_guarded: unit t          (* 8, used to be 25 *)
  | Unused_var: string t                 (* 26 *)
  | Unused_var_strict: string t          (* 27 *)
  | Wildcard_arg_to_constant_constr: unit t (* 28 *)
  | Eol_in_string: unit t                (* 29 *)
  | Duplicate_definitions: (string * string * string * string) t (* 30 *)
  | Multiple_definition: (string * string * string) t (* 31 *)
  | Unused_value_declaration: string t   (* 32 *)
  | Unused_open: string t                (* 33 *)
  | Unused_type_declaration: string t    (* 34 *)
  | Unused_for_index: string t           (* 35 *)
  | Unused_ancestor: string t            (* 36 *)
  | Unused_constructor: (string * bool * bool) t (* 37 *)
  | Unused_extension: (string * bool * bool * bool) t (* 38 *)
  | Unused_rec_flag: unit t              (* 39 *)
  | Name_out_of_scope: (string * string list * bool) t (* 40 *)
  | Ambiguous_name: (string list * string list *  bool * string) t (* 41 *)
  | Disambiguated_name: string t         (* 42 *)
  | Nonoptional_label: string t          (* 43 *)
  | Open_shadow_identifier: (string * string) t (* 44 *)
  | Open_shadow_label_constructor: (string * string) t (* 45 *)
  | Bad_env_variable: (string * string) t (* 46 *)
  | Attribute_payload: (string * string) t (* 47 *)
  | Eliminated_optional_arguments: string list t (* 48 *)
  | No_cmi_file: (string * string option) t (* 49 *)
  | Bad_docstring: bool t                (* 50 *)
  | Expect_tailcall: unit t              (* 51 *)
  | Fragile_literal_pattern: unit t      (* 52 *)
  | Misplaced_attribute: string t        (* 53 *)
  | Duplicated_attribute: string t       (* 54 *)
  | Inlining_impossible: string t        (* 55 *)
  | Unreachable_case: unit t             (* 56 *)
  | Ambiguous_pattern: string list t     (* 57 *)
  | No_cmx_file: string t                (* 58 *)
  | Assignment_to_non_mutable_value: unit t (* 59 *)
  | Unused_module: string t              (* 60 *)
  | Unboxable_type_in_prim_decl: string t (* 61 *)
  | Constraint_on_gadt: unit t           (* 62 *)
  | Erroneous_printed_signature: string t (* 63 *)
  | Unsafe_without_parsing: unit t       (* 64 *)
  | Redefining_unit: string t            (* 65 *)
  | Unused_open_bang: string t           (* 66 *)
  | Unused_functor_parameter: string t   (* 67 *)
;;

type alert = {kind:string; message:string; def:loc; use:loc}

val parse_options : bool -> string -> unit;;

val parse_alert_option: string -> unit
  (** Disable/enable alerts based on the parameter to the -alert
      command-line option.  Raises [Arg.Bad] if the string is not a
      valid specification.
  *)

val without_warnings : (unit -> 'a) -> 'a
  (** Run the thunk with all warnings and alerts disabled. *)

val is_active : _ t -> bool;;
val is_error : _ t -> bool;;

val defaults_w : string;;
val defaults_warn_error : string;;

type reporting_information =
  { id : string
  ; message : string
  ; is_error : bool
  ; sub_locs : (loc * string) list;
  }

val report : 'a t -> 'a -> [ `Active of reporting_information | `Inactive ]
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
