(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type 'a t = { mutable c : 'a list; mutable len : int; }

exception Empty

let create () = { c = []; len = 0; }

let clear s = s.c <- []; s.len <- 0

let copy s = { c = s.c; len = s.len; }

let push x s = s.c <- x :: s.c; s.len <- s.len + 1

let pop_opt s =
  match s.c with
  | hd::tl -> s.c <- tl; s.len <- s.len - 1; Some hd
  | []     -> None

let pop s =
  match pop_opt s with
  | None -> raise Empty
  | Some x -> x

let top_opt s =
  match s.c with
  | hd::_ -> Some hd
  | []    -> None

let top s =
  match top_opt s with
  | None -> raise Empty
  | Some x -> x

let is_empty s = (s.c = [])

let length s = s.len

let iter f s = List.iter f s.c

let fold f acc s = List.fold_left f acc s.c

(** {6 Iterators} *)

let to_seq s = List.to_seq s.c

let add_seq q i = Seq.iter (fun x -> push x q) i

let of_seq g =
  let s = create() in
  add_seq s g;
  s
