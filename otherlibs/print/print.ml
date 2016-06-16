(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Nicolas Ojeda Bar, LexiFi                       *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Printing of values *)

module EvalPath = struct
  type valu = Obj.t
  let eval_path _ _ = assert false
  exception Error
  let same_value v1 v2 = v1 == v2
end

let print_value _ _ = ()

module Printval = Genprintval.Make (Obj) (EvalPath)

let max_printer_steps = ref 10
let max_printer_depth = ref 10

let internal_print_value str ppf v =
  let env, te = Marshal.from_string str 0 in
  let oval =
    Printval.outval_of_value !max_printer_steps !max_printer_depth
      (fun _ _ _ -> None) env v te
  in
  !(Oprint.out_value) ppf oval
