(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Entry points in the parser *)

val implementation: Warnings.state -> Lexing.lexbuf -> Parsetree.structure
val interface: Warnings.state -> Lexing.lexbuf -> Parsetree.signature
val toplevel_phrase: Warnings.state -> Lexing.lexbuf -> Parsetree.toplevel_phrase
val use_file: Warnings.state -> Lexing.lexbuf -> Parsetree.toplevel_phrase list
val core_type: Warnings.state -> Lexing.lexbuf -> Parsetree.core_type
val expression: Warnings.state -> Lexing.lexbuf -> Parsetree.expression
val pattern: Warnings.state -> Lexing.lexbuf -> Parsetree.pattern
