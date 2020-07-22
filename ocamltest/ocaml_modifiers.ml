(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Definition of a few OCaml-specific environment modifiers *)

open Ocamltest_stdlib
open Environments

let principal =
[
  Append (Ocaml_variables.flags, " -principal ");
  Add (Ocaml_variables.compiler_directory_suffix, ".principal");
  Add (Ocaml_variables.compiler_reference_suffix, ".principal");
]

let latex =
  [
    Add (Ocaml_variables.ocamldoc_backend, "latex");
    Append (Ocaml_variables.ocamldoc_flags, "-latex-type-prefix=TYP ");
    Append (Ocaml_variables.ocamldoc_flags, "-latex-module-prefix= ");
    Append (Ocaml_variables.ocamldoc_flags, "-latex-value-prefix=  ");
    Append (Ocaml_variables.ocamldoc_flags, "-latex-module-type-prefix= ");
    Append (Ocaml_variables.ocamldoc_flags, "-latextitle=1,subsection* ");
    Append (Ocaml_variables.ocamldoc_flags, "-latextitle=2,subsubsection* ");
    Append (Ocaml_variables.ocamldoc_flags, "-latextitle=6,subsection* ");
    Append (Ocaml_variables.ocamldoc_flags, "-latextitle=7,subsubsection* ");
  ]


let html =
  [
    Add (Ocaml_variables.ocamldoc_backend, "html");
    Append (Ocaml_variables.ocamldoc_flags, "-colorize-code ");
  ]

let man =
  [
    Add (Ocaml_variables.ocamldoc_backend, "man");
  ]

let wrap strs = " " ^ String.concat " " strs ^ " "

let make_library_modifier library directories =
[
  Append (Ocaml_variables.directories, (wrap directories));
  Append (Ocaml_variables.libraries, (wrap [library]));
  Append (Ocaml_variables.caml_ld_library_path, (wrap directories));
]

let make_module_modifier unit_name directory =
[
  Append (Ocaml_variables.directories, (wrap [directory]));
  Append (Ocaml_variables.binary_modules, (wrap [unit_name]));
]

let compiler_subdir subdir =
  Filename.make_path (Ocamltest_config.ocamlsrcdir :: subdir)

let config =
[
  Append (Ocaml_variables.directories, (wrap [compiler_subdir ["utils"]]));
]

let testing = make_library_modifier
  "testing" [compiler_subdir ["testsuite"; "lib"]]

let tool_ocaml_lib = make_module_modifier
  "lib" (compiler_subdir ["testsuite"; "lib"])

let unixlibdir = if Sys.win32 then "win32unix" else "unix"

let unix = make_library_modifier
  "unix" [compiler_subdir ["otherlibs"; unixlibdir]]

let dynlink =
  make_library_modifier "dynlink"
    [compiler_subdir ["otherlibs"; "dynlink"];
     compiler_subdir ["otherlibs"; "dynlink"; "native"]]

let str = make_library_modifier
  "str" [compiler_subdir ["otherlibs"; "str"]]

let systhreads =
  unix @
  (make_library_modifier
    "threads" [compiler_subdir ["otherlibs"; "systhreads"]])

let compilerlibs_subdirs =
[
  "asmcomp";
  "bytecomp";
  "compilerlibs";
  "driver";
  "file_formats";
  "lambda";
  "middle_end";
  "parsing";
  "toplevel";
  "typing";
  "utils";
]

let add_compiler_subdir subdir =
  Append (Ocaml_variables.directories, (wrap [compiler_subdir [subdir]]))

let compilerlibs_archive archive =
  (Append (Ocaml_variables.libraries, wrap [archive])) ::
  (List.map add_compiler_subdir compilerlibs_subdirs)

let debugger = [add_compiler_subdir "debugger"]

let _ =
  register_modifiers "principal" principal;
  register_modifiers "config" config;
  register_modifiers "testing" testing;
  register_modifiers "unix" unix;
  register_modifiers "dynlink" dynlink;
  register_modifiers "str" str;
  List.iter
    (fun archive -> register_modifiers archive (compilerlibs_archive archive))
    [
      "ocamlcommon";
      "ocamlbytecomp";
      "ocamlmiddleend";
      "ocamloptcomp";
      "ocamltoplevel";
    ];
  register_modifiers "systhreads" systhreads;
  register_modifiers "latex" latex;
  register_modifiers "html" html;
  register_modifiers "man" man;
  register_modifiers "tool-ocaml-lib" tool_ocaml_lib;
  register_modifiers "debugger" debugger;
  ()
