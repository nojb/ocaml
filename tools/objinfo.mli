(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Sebastien Hinderer, Tarides, Paris                   *)
(*                                                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

val print_general_infos :
  string ->
  Digest.t ->
  string list ->
  (string * Digest.t option) list ->
  (string * Digest.t option) list -> unit

val print_spaced_string : string -> unit

val exit_errf : ('a, unit, string, 'b) format4 -> 'a

val main :
  cmx:(Misc.Magic_number.native_obj_config -> in_channel -> unit) ->
  cmxa:(Misc.Magic_number.native_obj_config -> in_channel -> unit) ->
  (string * Arg.spec * string) list -> unit
