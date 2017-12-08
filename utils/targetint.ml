(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                        Nicolas Ojeda Bar, LexiFi                       *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type repr =
  | Int32 of int32
  | Int64 of int64

module type INT = sig
  type t
  val zero : t
  val one : t
  val minus_one : t
  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val succ : t -> t
  val pred : t -> t
  val abs : t -> t
  val size : int
  val max_int : t
  val min_int : t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t
  val of_int : int -> t
  val of_int_exn : int -> t
  val to_int : t -> int
  val of_float : float -> t
  val to_float : t -> float
  val of_int32 : int32 -> t
  val to_int32 : t -> int32
  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val of_string : string -> t
  val to_string : t -> string
  val compare: t -> t -> int
  val equal: t -> t -> bool
end

module Int32 = struct
  include Int32
  let of_int_exn =
    match Sys.word_size with (* size of [int] *)
    | 32 ->
        Int32.of_int
    | 64 ->
        fun n ->
          if n < Int32.(to_int min_int) || n > Int32.(to_int max_int) then
            Misc.fatal_errorf "Targetint.of_int_exn: 0x%x out of range" n
          else
            Int32.of_int n
    | _ ->
        assert false
  let of_int32 x = x
  let to_int32 x = x
  let of_int64 = Int64.to_int32
  let to_int64 = Int64.of_int32
  let repr x = Int32 x
  let size = 32
end

module Int64 = struct
  include Int64
  let of_int_exn = Int64.of_int
  let of_int64 x = x
  let to_int64 x = x
  let repr x = Int64 x
  let size = 64
end

module type NATIVE_INT = sig
  include INT
  val repr: t -> repr
end

let target_size = Sys.word_size
(* Later, this will be set by the configure script
   in order to support cross-compilation. *)

include (val
          (match target_size with
           | 32 -> (module Int32)
           | 64 -> (module Int64)
           | _ -> assert false
          ) : NATIVE_INT)

module MakeOCaml (I : INT) : INT = struct
  type t = I.t
  let check =
    let min_int = I.shift_right I.min_int 1 in
    let max_int = I.shift_right I.max_int 1 in
    fun n ->
      if n < min_int || n > max_int then
        Misc.fatal_errorf "Targetint.check: %s out of range" (I.to_string n)
      else
        n
  let mask = I.shift_left I.minus_one 1
  let of_int n = I.shift_left (I.of_int n) 1
  let zero = of_int 0
  let one = of_int 1
  let minus_one = of_int (-1)
  let add = I.add
  let sub = I.sub
  let neg = I.neg
  let abs = I.abs
  let succ a = add a one
  let pred a = sub a one
  let shift_left = I.shift_left
  let shift_right x i = I.logand mask (I.shift_right x i)
  let shift_right_logical x i = I.logand mask (I.shift_right_logical x i)
  let mul a b = I.mul a (I.shift_right b 1)
  let equal = I.equal
  let compare = I.compare
  let to_string x = I.to_string (I.shift_right x 1)
  let of_string s = I.shift_left (check (I.of_string s)) 1
  let to_int64 x = I.to_int64 (I.shift_right x 1)
  let of_int64 x = I.shift_left (I.of_int64 x) 1
  let to_int32 x = I.to_int32 (I.shift_right x 1)
  let of_int32 x = I.shift_left (I.of_int32 x) 1
  let to_float x = I.to_float (I.shift_right x 1)
  let of_float x = I.shift_left (I.of_float x) 1
  let to_int x = I.to_int (I.shift_right x 1)
  let lognot x = I.logand mask (I.lognot x)
  let logxor a b = I.logand mask (I.logxor a b)
  let logor = I.logor
  let logand = I.logand
  let max_int = I.logand mask I.max_int
  let min_int = I.min_int
  let rem a b = I.rem a b
  let div a b = I.shift_left (I.div a b) 1
  let of_int_exn n = I.shift_left (check (I.of_int_exn n)) 1
  let size = I.size - 1
end

module OCaml =
  (val (match size with
       | 32 -> (module MakeOCaml (Int32))
       | 64 -> (module MakeOCaml (Int64))
       | _ -> assert false
     ) : INT)
