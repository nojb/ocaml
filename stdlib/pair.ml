(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2023 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type ('a, 'b) t = 'a * 'b

let create a b = (a, b)

let fst (a, _) = a

let snd (_, b) = b

let equal fa fb (a, b) (a', b') =
  fa a a' && fb b b'

let compare fa fb (a, b) (a', b') =
  let c = fa a a' in if c <> 0 then c else fb b b'

let swap (a, b) = (b, a)

let map fa fb (a, b) = (fa a, fb b)

let iter fa fb (a, b) = fa a; fb b

let fold f (a, b) = f a b
