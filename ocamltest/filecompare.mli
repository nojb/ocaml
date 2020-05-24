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

(* File comparison tools *)

type result =
  | Same
  | Different
  | Unexpected_output
  | Error of string * int

type tool

val make_comparison_tool :
  ?result_of_exitcode:(string -> int -> result) ->
  ?flags:string -> string -> tool

val default_comparison_tool : tool

type filetype = Binary of {skip_bytes: int} | Text of {skip_lines: int}

type files = {
  filetype : filetype;
  reference_filename : string;
  output_filename : string;
}

val compare_files : ?tool:tool -> files -> result

val check_file : ?tool:tool -> files -> result

val cmp_result_of_exitcode : string -> int -> result

val diff : files -> (string, string) Stdlib.result

val promote : files -> unit
