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

(* Build libraries of .cmo files *)

(* Format of a library file:
      magic number (Config.cma_magic_number)
      absolute offset of content table
      blocks of relocatable bytecode
      content table = list of compilation units
*)

(* Format of a thin library file (-a -thin):
      magic number (Config.cma_thin_magic_number)
      content table = list of paths of the member .cmo files
*)

val create_archive: string list -> string -> unit

type error =
    File_not_found of string
  | Not_an_object_file of string
  | Full_archive_member of string
  | Link_error of Linkdeps.error

exception Error of error

val report_error: error Format_doc.format_printer
val report_error_doc: error Format_doc.printer
val reset: unit -> unit
