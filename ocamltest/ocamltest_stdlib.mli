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

(* A few extensions to OCaml's standard library *)

(* Pervasive *)

val initial_cwd : string

val relative_to_initial_cwd : string -> string

val input_line_opt : in_channel -> string option

module Char : sig
  include module type of Char
  val is_blank : char -> bool
end

module Filename  : sig
  include module type of Filename
  val path_sep : string
  val maybe_quote : string -> string
  val make_filename : string -> string -> string
  val make_path : string list -> string
  val mkexe : string -> string
end

module String : sig
  include module type of Misc.Stdlib.String
  val words : string -> string list
end

module Sys : sig
  include module type of Sys
  val file_is_empty : string -> bool
  val run_system_command : string -> unit
  val make_directory : string -> unit
  val string_of_file : string -> string
  val copy_chan : in_channel -> out_channel -> unit
  val copy_file : string -> string -> unit
  val force_remove : string -> unit
  val has_symlink : unit -> bool
  val with_chdir : string -> (unit -> 'a) -> 'a
  val getenv_with_default_value : string -> string -> string
  val safe_getenv : string -> string
end
