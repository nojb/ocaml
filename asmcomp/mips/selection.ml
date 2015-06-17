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

(* Instruction selection for the Mips processor *)

open Cmm
open Arch
open Mach

class selector = object(self)

inherit Selectgen.selector_generic as super

method! regs_for tyv =
  Reg.createv (
                 (* Expand floats into pairs of integer registers *)
                 let rec expand = function
                   [] -> []
                 | Float :: tyl -> Int :: Int :: expand tyl
                 | ty :: tyl -> ty :: expand tyl in
                 Array.of_list (expand (Array.to_list tyv))
  )

method is_immediate (n : int) = true

method select_addressing chunk = function
    Cconst_symbol s ->
      (Ibased(s, 0), Ctuple [])
  | Cop(Cadda, [Cconst_symbol s; Cconst_int n]) ->
      (Ibased(s, n), Ctuple [])
  | Cop(Cadda, [arg; Cconst_int n]) ->
      (Iindexed n, arg)
  | Cop(Cadda, [arg1; Cop(Caddi, [arg2; Cconst_int n])]) ->
      (Iindexed n, Cop(Cadda, [arg1; arg2]))
  | arg ->
      (Iindexed 0, arg)

method! select_operation op args =
  match (op, args) with
  (* Turn floating-point operations into runtime ABI calls *)
  | (Caddf, args) -> (Iextcall("__adddf3", false), args)
  | (Csubf, args) -> (Iextcall("__subdf3", false), args)
  | (Cmulf, args) -> (Iextcall("__muldf3", false), args)
  | (Cdivf, args) -> (Iextcall("__divdf3", false), args)
  | (Cnegf, args) -> (Iextcall("__negdf2", false), args)
  | (Cfloatofint, args) -> (Iextcall("__floatsidf", false), args)
  | (Cintoffloat, args) -> (Iextcall("__fixdfsi", false), args)
  | (Ccmpf comp, args) ->
      let func, tst = match comp with
        | Cne -> "__nedf2", Iunsigned Cne
        | Ceq -> "__eqdf2", Iunsigned Ceq
        | Clt -> "__ltdf2", Isigned Clt
        | Cle -> "__ledf2", Isigned Cle
        | Cgt -> "__gtdf2", Isigned Cgt
        | Cge -> "__gedf2", Isigned Cge
      in
      (Iintop_imm (Icomp tst, 0),
       [Cop(Cextcall(func, typ_int, false, Debuginfo.none), args)])
  (* Add coercions around loads and stores of 32-bit floats *)
  | (Cload Single, args) ->
      (Iextcall("__extendsfdf2", false), [Cop(Cload Word, args)])
  | (Cstore Single, [arg1; arg2]) ->
      let arg2' =
        Cop(Cextcall("__truncdfsf2", typ_int, false, Debuginfo.none),
            [arg2]) in
      self#select_operation (Cstore Word) [arg1; arg2']
  (* Other operations are regular *)
  | (op, args) -> super#select_operation op args

method! select_condition = function
  (* Turn floating-point comparisons into runtime ABI calls *)
    Cop(Ccmpf comp, args) ->
      let func, tst = match comp with
        | Cne -> "__nedf2", Itruetest
        | Ceq -> "__eqdf2", Ifalsetest
        | Clt -> "__ltdf2", Iinttest_imm(Isigned Clt, 0)
        | Cle -> "__ledf2", Iinttest_imm(Isigned Cle, 0)
        | Cgt -> "__gtdf2", Iinttest_imm(Isigned Cgt, 0)
        | Cge -> "__gedf2", Iinttest_imm(Isigned Cge, 0)
      in
      (tst, Cop(Cextcall(func, typ_int, false, Debuginfo.none), args))
  | expr ->
      super#select_condition expr

end

let fundecl f = (new selector)#emit_fundecl f
