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

open Printf

let to_string = Exn.to_string

let print fct arg =
  try
    fct arg
  with x ->
    eprintf "Uncaught exception: %s\n" (to_string x);
    flush stderr;
    raise x

let catch fct arg =
  try
    fct arg
  with x ->
    flush stdout;
    eprintf "Uncaught exception: %s\n" (to_string x);
    exit 2

type raw_backtrace_slot = Stack_trace.Slot.Raw.t
type raw_backtrace = Stack_trace.t

let get_raw_backtrace = Exn.last_trace

external raise_with_backtrace: exn -> raw_backtrace -> 'a
  = "%raise_with_backtrace"

type backtrace_slot = Stack_trace.Slot.t

let convert_raw_backtrace_slot = Stack_trace.Slot.of_raw

let print_raw_backtrace outchan raw_backtrace =
  output_string outchan (Stack_trace.to_string raw_backtrace)

(* confusingly named: prints the global current backtrace *)
let print_backtrace outchan =
  output_string outchan (Stack_trace.to_string (Exn.last_trace ()))

let raw_backtrace_to_string = Stack_trace.to_string

type location = Stack_trace.Slot.location = {
  filename : string;
  line_number : int;
  start_char : int;
  end_char : int;
}

let backtrace_slots = Stack_trace.slots

module Slot = struct
  type t = Stack_trace.Slot.t
  let format = Stack_trace.Slot.format
  let is_raise = Stack_trace.Slot.is_raise
  let is_inline = Stack_trace.Slot.is_inline
  let location = Stack_trace.Slot.location
end

let raw_backtrace_length = Stack_trace.length
let get_raw_backtrace_slot = Stack_trace.Slot.Raw.get
let get_raw_backtrace_next_slot = Stack_trace.Slot.Raw.next

(* confusingly named:
   returns the *string* corresponding to the global current backtrace *)
let get_backtrace () = Stack_trace.to_string (Exn.last_trace ())

let record_backtrace = Exn.set_tracing
let backtrace_status = Exn.tracing
let register_printer = Exn.register_printer
let get_callstack = Stack_trace.current
let exn_slot_id = Exn.id
let exn_slot_name = Exn.name
let set_uncaught_exception_handler = Exn.set_uncaught_exception_handler
