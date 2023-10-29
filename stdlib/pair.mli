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

(** Pairs.

    @since 5.2 *)

type ('a, 'b) t = 'a * 'b
(** The type of pairs. *)

val create: 'a -> 'b -> ('a, 'b) t
(** [create a b] is the pair [(a, b)]. *)

val fst: ('a, 'b) t -> 'a
(** [fst v] is the first component of [v]. *)

val snd: ('a, 'b) t -> 'b
(** [snd v] is the second component of [v]. *)

val equal: ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
(** [equal fa fb v1 v2] returns whether [v1] is equal to [v2]. The two pairs are
    considered equal if their first components are equal according to [fa] and
    their second components are equal according to [fb]. *)

val compare: ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
(** [compare fa fb v1 v2] compares the two pairs [v1] and [v2] lexicographically
    using [fa] to compare the first component of the pairs and [fb] for the
    second component. *)

val swap: ('a, 'b) t -> ('b, 'a) t
(** [swap (a, b)] is [(b, a)]. *)

val map: ('a1 -> 'a2) -> ('b1 -> 'b2) -> ('a1, 'b1) t -> ('a2, 'b2) t
(** [map fa fb (a, b)] is [(fa a, fb b)]. *)

val iter: ('a -> unit) -> ('b -> unit) -> ('a, 'b) t -> unit
(** [iter fa fb (a, b)] performs [fa a] followed by [fb b]. *)

val fold: ('a -> 'b -> 'c) -> ('a, 'b) t -> 'c
(** [fold f (a, b)] is [f a b]. *)
