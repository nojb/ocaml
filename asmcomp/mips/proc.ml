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

(* Description of the Mips processor *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(* Instruction selection *)

let word_addressed = false

(* Registers available for register allocation *)

(* Register map:
    $0                          always 0
    $1                          temporary for the assembler
    $2 - $7     0 - 5           function results
    $8 - $15    6 - 13          function arguments
    $16 - $21   14 - 19         general purpose (preserved by C)
    $22                         allocation pointer (preserved by C)
    $23                         allocation limit (preserved by C)
    $24 - $25                   temporaries
    $26 - $29                   kernel regs, stack pointer, global pointer
    $30                         trap pointer (preserved by C)
    $31                         return address *)

let int_reg_name = [|
  (* 0-5 *)    "$2"; "$3"; "$4"; "$5"; "$6"; "$7";
  (* 6-13 *)   "$8"; "$9"; "$10"; "$11"; "$12"; "$13"; "$14"; "$15";
  (* 14-19 *)  "$16"; "$17"; "$18"; "$19"; "$20"; "$21"
|]

let num_register_classes = 1

let register_class r = assert (r.typ <> Float); 0

let num_available_registers = [| 20 |]

let first_available_register = [| 0 |]

let register_name r = int_reg_name.(r)

let rotate_registers = true

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.make 20 Reg.dummy in
  for i = 0 to 19 do v.(i) <- Reg.at_location Int (Reg i) done;
  v

let all_phys_regs = hard_int_reg

let phys_reg n = hard_int_reg.(n)

let stack_slot slot ty =
  assert (ty <> Float);
  Reg.at_location ty (Stack slot)

(* Calling conventions *)

let calling_conventions first_int last_int initial_stack_ofs make_stack arg =
  let loc = Array.make (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let ofs = ref initial_stack_ofs in
  for i = 0 to Array.length arg - 1 do
    match arg.(i).typ with
      Int | Addr as ty ->
        if !int <= last_int then begin
          loc.(i) <- phys_reg !int;
          incr int
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) ty;
          ofs := !ofs + size_int
        end
    | Float ->
        assert false
  done;
  (loc, Misc.align !ofs 8)         (* Keep stack 8-aligned *)

let incoming ofs = Incoming ofs
let outgoing ofs = Outgoing ofs
let not_supported ofs = fatal_error "Proc.loc_results: cannot call"

let loc_arguments arg =
  calling_conventions 6 13 0 outgoing arg
let loc_parameters arg =
  let (loc, ofs) = calling_conventions 6 13 0 incoming arg in loc
let loc_results res =
  let (loc, ofs) = calling_conventions 0 5 0 not_supported res in loc

(* The C compiler calling conventions are peculiar, especially when
   a mix of ints and floats are passed. Here, we trap a few special
   cases and revert to [calling_conventions] when only scalars are
   passed. *)

let loc_external_arguments arg =
  calling_conventions 2 5 16 outgoing arg

let loc_external_results res =
  let (loc, ofs) = calling_conventions 0 1 0 not_supported res in loc

let loc_exn_bucket = phys_reg 0         (* $2 *)

(* Volatile registers: none *)

let regs_are_volatile rs = false

(* Registers destroyed by operations *)

let destroyed_at_c_call =               (* $16 - $21 preserved *)
  Array.of_list(List.map phys_reg
    [0;1;2;3;4;5;6;7;8;9;10;11;12;13])

let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _ | Iextcall(_, true)) -> all_phys_regs
  | Iop(Iextcall(_, false)) -> destroyed_at_c_call
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure *)

let safe_register_pressure = function
    Iextcall(_, _) -> 6
  | _ -> 20
let max_register_pressure = function
    Iextcall(_, _) -> [| 6 |]
  | _ -> [| 20 |]

(* Pure operations (without any side effect besides updating their result
   registers). *)

let op_is_pure = function
  | Icall_ind | Icall_imm _ | Itailcall_ind | Itailcall_imm _
  | Iextcall _ | Istackoffset _ | Istore _ | Ialloc _
  | Iintop(Icheckbound) | Iintop_imm(Icheckbound, _) -> false
  | _ -> true

(* Layout of the stack *)

let num_stack_slots = [| 0 |]
let contains_calls = ref false

(* Calling the assembler *)

let assemble_file infile outfile =
  Ccomp.command (Config.asm ^ " --no-warn -o " ^
                 Filename.quote outfile ^ " " ^ Filename.quote infile)

let init () = ()
