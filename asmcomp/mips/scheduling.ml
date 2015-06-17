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

let _ = let module M = Schedgen in () (* to create a dependency *)

(* No scheduling is needed for the Mips, the assembler
   does it better than us.  *)

let fundecl f = f
