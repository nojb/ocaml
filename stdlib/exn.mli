(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Exception values.

    Facilities for printing exceptions and inspecting current call stack.

    @since 4.08 *)

type t = exn = ..
(** The type for exceptions. *)

val to_string: t -> string
(** [to_string e] returns a string representation of the exception [e]. *)

module Backtrace : sig
  (** {1 Raw backtraces} *)

  type exn

  type t
  (** The abstract type [t] stores a backtrace.

      Raw backtraces cannot be marshalled. If you need marshalling, you should
      use the array returned by the {!slots} function. *)

  val set_recording_status: bool -> unit
  (** [set_recording_status b] turns recording of exception backtraces on (if [b
      = true]) or off (if [b = false]).  Initially, backtraces are not recorded,
      unless the [b] flag is given to the program through the [OCAMLRUNPARAM]
      variable. *)

  val get_recording_status: unit -> bool
  (** Returns [true] if exception backtraces are currently recorded, [false] if
      not. *)

  val current: unit -> t
  (** Returns a backtrace containing the program locations where the
      most-recently raised exception was raised and where it was propagated
      through function calls.

      If the call is not inside an exception handler, the returned backtrace is
      unspecified. If the call is after some exception-catching code (before in
      the handler, or in a when-guard during the matching of the exception
      handler), the backtrace may correspond to a later exception than the
      handled one. *)

  val to_string: t -> string
  (** Return a string from a backtrace, listing the program locations contained
      in the backtrace. *)

  val length: t -> int
  (** [length bckt] returns the number of slots in the backtrace [bckt].  *)

  module Slot : sig
    (** {1 Manipulation of backtrace information}

        These functions are used to traverse the slots of a raw backtrace and
        extract information from them in a programmer-friendly format. *)

    type backtrace

    type t
    (** The abstract type [t] represents a single slot of a backtrace. *)

    val is_raise: t -> bool
    (** [is_raise slot] is [true] when [slot] refers to a raising
        point in the code, and [false] when it comes from a simple
        function call. *)

    val is_inline: t -> bool
    (** [is_inline slot] is [true] when [slot] refers to a call
        that got inlined by the compiler, and [false] when it comes from
        any other context. *)

    type location =
      { filename: string;
        line_number: int;
        start_char: int;
        end_char: int }
    (** The type of location information found in backtraces. [start_char]
        and [end_char] are positions relative to the beginning of the
        line. *)

    val location: t -> location option
    (** [location slot] returns the location information of the slot,
        if available, and [None] otherwise.

        Some possible reasons for failing to return a location are as follow:
        - the slot corresponds to a compiler-inserted raise
        - the slot corresponds to a part of the program that has not been
        compiled with debug information ([-g]) *)

    val format: int -> t -> string option
    (** [format pos slot] returns the string representation of [slot] as
        {!Raw_backtrace.to_string] would format it, assuming it is the [pos]-th
        element of the backtrace: the [0]-th element is pretty-printed differently
        than the others.

        Whole-backtrace printing functions also skip some uninformative
        slots; in that case, [format pos slot] returns [None]. *)

    module Raw : sig
      (** {1 Raw backtrace slots} *)

      type t
      (** This type allows direct access to raw backtrace slots, without any
          conversion in an OCaml-usable data-structure. Being
          process-specific, they must absolutely not be marshalled, and are
          unsafe to use for this reason (marshalling them may not fail, but
          un-marshalling and using the result will result in
          undefined behavior).

          Elements of this type can still be compared and hashed: when two
          elements are equal, then they represent the same source location
          (the converse is not necessarily true in presence of inlining,
          for example). *)

      val get: backtrace -> int -> t
      (** [get bckt pos] returns the slot in position [pos] in the backtrace
          [bckt]. *)

      val next: t -> t option
      (** [next slot] returns the next slot inlined, if any.

          Sample code to iterate over all frames (inlined and non-inlined):
          {[
            (* Iterate over inlined frames *)
            let rec iter_backtrace_slot f slot =
              f slot;
              match get slot with
              | None -> ()
              | Some slot' -> iter_backtrace_slot f slot'

            (* Iterate over stack frames *)
            let iter_backtrace f bt =
              for i = 0 to length bt - 1 do
                iter_backtrace_slot f (get bt i)
              done
          ]}
      *)
    end

    val of_raw: Raw.t -> t
    (** Extracts the user-friendly backtrace slot from a low-level
        raw backtrace slot. *)
  end with type backtrace := t

  val slots: t -> Slot.t array option
  (** Returns the slots of a raw backtrace, or [None] if none of them
      contain useful information.

      In the return array, the slot at index [0] corresponds to the most recent
      function call, raise, or primitive {!current} call in the trace.

      Some possible reasons for returning [None] are as follow:
      - none of the slots in the trace come from modules compiled with
      debug information ([-g])
      - the program is a bytecode program that has not been linked with
      debug information enabled ([ocamlc -g]) *)
end with type exn := t

external raise: t -> 'a = "%raise"

external reraise: t -> 'a = "%reraise"

external raise_notrace: t -> 'a = "%raise_notrace"

external raise_with_backtrace: t -> Backtrace.t -> 'a = "%raise_with_backtrace"
(** Reraise the exception using the given raw backtrace for the origin of the
    exception. *)

(** {1 Current call stack} *)

val current_callstack: int -> Backtrace.t
(** [current_callstack n] returns a description of the top of the call stack on
    the current program point (for the current thread), with at most [n]
    entries.  (Note: this function is not related to exceptions at all, despite
    being part of the {!Exn} module.) *)

val register_printer: (t -> string option) -> unit
(** [register_printer fn] registers [fn] as an exception printer.  The printer
    should return [None] or raise an exception if it does not know how to
    convert the passed exception, and [Some s] with [s] the resulting string if
    it can convert the passed exception. Exceptions raised by the printer are
    ignored.

    When converting an exception into a string, the printers will be invoked in
    the reverse order of their registrations, until a printer returns a [Some s]
    value (if no such printer exists, the runtime will use a generic printer).

    When using this mechanism, one should be aware that an exception backtrace
    is attached to the thread that saw it raised, rather than to the exception
    itself. Practically, it means that the code related to [fn] should not use
    the backtrace if it has itself raised an exception before. *)

(** {1 Uncaught exceptions} *)

val set_uncaught_exception_handler: (t -> Backtrace.t -> unit) -> unit
(** [Printexc.set_uncaught_exception_handler fn] registers [fn] as the handler
    for uncaught exceptions. The default handler prints the exception and
    backtrace on standard error output.

    Note that when [fn] is called all the functions registered with
    {!Stdlib.at_exit} have already been called. Because of this you must
    make sure any output channel [fn] writes on is flushed.

    Also note that exceptions raised by user code in the interactive toplevel
    are not passed to this function as they are caught by the toplevel itself.

    If [fn] raises an exception, both the exceptions passed to [fn] and raised
    by [fn] will be printed with their respective backtrace. *)

(** {1 Exception slots} *)

val id: t -> int
(** [id exn] returns an integer which uniquely identifies the constructor used
    to create the exception value [exn] (in the current runtime).  *)

val name: t -> string
(** [name exn] returns the internal name of the constructor used to create the
    exception value [exn].  *)
