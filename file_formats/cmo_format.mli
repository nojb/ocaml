(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2006 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Symbol table information for .cmo and .cma files *)

type modname = string
type crcs = (modname * Digest.BLAKE128.t option) list

(* Names of compilation units as represented in CMO files *)
type compunit = Compunit of string [@@unboxed]

(* Predefined symbols as represented in CMO files *)

type predef =
  | Predef_exn of string [@@unboxed]

(* Relocation information *)

type reloc_info =
  | Reloc_literal of Obj.t (* structured constant *)
  | Reloc_getcompunit of compunit (* reference to a compunit *)
  | Reloc_getpredef of predef (* reference to a predef *)
  | Reloc_setcompunit of compunit (* definition of a compunit *)
  | Reloc_primitive of string (* C primitive number *)

(* Descriptor for compilation units *)

type compilation_unit =
  { cu_name: compunit;                   (* Name of compilation unit *)
    mutable cu_pos: int;                (* Absolute position in file *)
    cu_codesize: int;                   (* Size of code block *)
    cu_reloc: (reloc_info * int) list;  (* Relocation information *)
    cu_imports: crcs;                     (* Names and CRC of intfs imported *)
    cu_required_compunits: compunit list; (* Compilation units whose
                                             initialization side effects
                                             must occur before this one. *)
    cu_primitives: string list;         (* Primitives declared inside *)
    mutable cu_force_link: bool;        (* Must be linked even if unref'ed *)
    mutable cu_debug: int;              (* Position of debugging info, or 0 *)
    cu_debugsize: int;                  (* Length of debugging info *)
    mutable cu_hint: int;               (* Position of hint info, or 0 *)
    cu_hintsize: int }                  (* Length of hint info *)

(* Format of a .cmo file:
     magic number (Config.cmo_magic_number)
     absolute offset of compilation unit descriptor
     block of relocatable bytecode
     debugging information if any
     hint information if any
     compilation unit descriptor *)

(* Descriptor for libraries *)

type library =
  { lib_units: compilation_unit list;   (* List of compilation units *)
    lib_custom: bool;                   (* Requires custom mode linking? *)
    (* In the following fields the lists are reversed with respect to
       how they end up being used on the command line. *)
    lib_ccobjs: string list;            (* C object files needed for -custom *)
    lib_ccopts: string list;            (* Extra opts to C compiler *)
    lib_dllibs: (suffixed:bool * string) list }  (* DLLs needed *)

(* Format of a .cma file:
     magic number (Config.cma_magic_number)
     absolute offset of library descriptor
     object code for first library member
     ...
     object code for last library member
     library descriptor *)

(* Descriptor for thin libraries.  A thin library does not contain a copy
   of its members: it only records where to find them. *)

type thin_unit =
  { tu_path: string;                    (* Path of the member .cmo file.
                                           A relative path is interpreted
                                           relative to the directory holding
                                           the .cma file. *)
    tu_force_link: bool }               (* Must be linked even if unref'ed,
                                           because the archive holding this
                                           member was built with -linkall *)

type thin_library =
  { tlib_units: thin_unit list;         (* Members of the library *)
    tlib_custom: bool;                  (* Requires custom mode linking? *)
    (* In the following fields the lists are reversed with respect to
       how they end up being used on the command line. *)
    tlib_ccobjs: string list;           (* C object files needed for -custom *)
    tlib_ccopts: string list;           (* Extra opts to C compiler *)
    tlib_dllibs: (suffixed:bool * string) list }  (* DLLs needed *)

(* Format of a thin .cma file:
     magic number (Config.cma_thin_magic_number)
     thin library descriptor *)
