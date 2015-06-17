(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                Nicolas Ojeda Bar <n.oje.bar@gmail.com>              *)
(*                                                                     *)
(*  Copyright 2013 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Specific operations for the Mips processor *)

open Misc
open Format

let command_line_options = []

(* Addressing modes *)

type addressing_mode =
    Ibased of string * int              (* symbol + displ *)
  | Iindexed of int                     (* reg + displ *)

(* Specific operations *)

type specific_operation = unit          (* none *)

(* Sizes, endianness *)

let big_endian = true

let size_addr = 4
let size_int = 4
let size_float = 8

let allow_unaligned_access = false

(* Behavior of division *)

let division_crashes_on_overflow = true

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
    Ibased(s, n) -> Ibased(s, n + delta)
  | Iindexed n -> Iindexed(n + delta)

let num_args_addressing = function
    Ibased(s, n) -> 0
  | Iindexed n -> 1

(* Printing operations and addressing modes *)

let print_addressing printreg addr ppf arg =
  match addr with
    Ibased(s, n) ->
      fprintf ppf "\"%s\"" s;
      if n <> 0 then begin fprintf ppf " + %d" n end
  | Iindexed n ->
      printreg ppf arg.(0);
      if n <> 0 then begin fprintf ppf " + %d" n end

let print_specific_operation printreg op ppf arg =
  fatal_error "Arch_mips.print_specific_operation"
