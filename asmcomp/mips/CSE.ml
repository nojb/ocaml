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

(* CSE for Mips32 *)

open Mach
open CSEgen

class cse = object (self)

inherit cse_generic

method! is_cheap_operation op =
  match op with
  | Iconst_int n | Iconst_blockheader n -> n <= 255n && n >= 0n
  | _ -> false

end

let fundecl f =
  (new cse)#fundecl f
