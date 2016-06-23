module type ARCH = sig
  (* Machine-specific command-line options *)
  val command_line_options: (string * Arg.spec * string) list

  (* Specific operations for the AMD64 processor *)
  type addressing_mode
  type specific_operation

  (* Sizes, endianness *)
  val big_endian: bool

  val size_addr: int
  val size_int: int
  val size_float: int

  val allow_unaligned_access: bool

  (* Behavior of division *)

  val division_crashes_on_overflow: bool

  (* Operations on addressing modes *)

  val identity_addressing: addressing_mode
  val offset_addressing: addressing_mode -> int -> addressing_mode
  val num_args_addressing: addressing_mode -> int

  (* Printing operations and addressing modes *)

  val print_addressing: (Format.formatter -> Reg.t -> unit) -> addressing_mode -> Format.formatter -> Reg.t array -> unit
  val print_specific_operation: (Format.formatter -> Reg.t -> unit) -> specific_operation -> Format.formatter -> Reg.t array -> unit
end

module type MACH = sig
  (* Representation of machine code by sequences of pseudoinstructions *)
  module Arch : ARCH

  type integer_comparison =
      Isigned of Cmm.comparison
    | Iunsigned of Cmm.comparison

  type integer_operation =
      Iadd | Isub | Imul | Imulh | Idiv | Imod
    | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
    | Icomp of integer_comparison
    | Icheckbound

  type test =
      Itruetest
    | Ifalsetest
    | Iinttest of integer_comparison
    | Iinttest_imm of integer_comparison * int
    | Ifloattest of Cmm.comparison * bool
    | Ioddtest
    | Ieventest

  type operation =
      Imove
    | Ispill
    | Ireload
    | Iconst_int of nativeint
    | Iconst_float of int64
    | Iconst_symbol of string
    | Iconst_blockheader of nativeint
    | Icall_ind
    | Icall_imm of string
    | Itailcall_ind
    | Itailcall_imm of string
    | Iextcall of string * bool    (* false = noalloc, true = alloc *)
    | Istackoffset of int
    | Iload of Cmm.memory_chunk * Arch.addressing_mode
    | Istore of Cmm.memory_chunk * Arch.addressing_mode * bool
    (* false = initialization, true = assignment *)
    | Ialloc of int
    | Iintop of integer_operation
    | Iintop_imm of integer_operation * int
    | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
    | Ifloatofint | Iintoffloat
    | Ispecific of Arch.specific_operation

  type instruction =
    { desc: instruction_desc;
      next: instruction;
      arg: Reg.t array;
      res: Reg.t array;
      dbg: Debuginfo.t;
      mutable live: Reg.Set.t }

  and instruction_desc =
      Iend
    | Iop of operation
    | Ireturn
    | Iifthenelse of test * instruction * instruction
    | Iswitch of int array * instruction array
    | Iloop of instruction
    | Icatch of int * instruction * instruction
    | Iexit of int
    | Itrywith of instruction * instruction
    | Iraise of Lambda.raise_kind

  type fundecl =
    { fun_name: string;
      fun_args: Reg.t array;
      fun_body: instruction;
      fun_fast: bool;
      fun_dbg : Debuginfo.t }

  val dummy_instr: instruction
  val end_instr: unit -> instruction
  val instr_cons:
    instruction_desc -> Reg.t array -> Reg.t array -> instruction ->
    instruction
  val instr_cons_debug:
    instruction_desc -> Reg.t array -> Reg.t array -> Debuginfo.t ->
    instruction -> instruction
  val instr_iter: (instruction -> unit) -> instruction -> unit
end

module type PRINTMACH = sig
  (* Pretty-printing of pseudo machine code *)

  module Mach : MACH
  open Format

  val reg: formatter -> Reg.t -> unit
  val regs: formatter -> Reg.t array -> unit
  val regset: formatter -> Reg.Set.t -> unit
  val regsetaddr: formatter -> Reg.Set.t -> unit
  val operation: Mach.operation -> Reg.t array -> formatter -> Reg.t array -> unit
  val test: Mach.test -> formatter -> Reg.t array -> unit
  val instr: formatter -> Mach.instruction -> unit
  val fundecl: formatter -> Mach.fundecl -> unit
  val phase: string -> formatter -> Mach.fundecl -> unit
  val interferences: formatter -> unit -> unit
  val preferences: formatter -> unit -> unit

  val print_live: bool ref
end

module type PROC = sig
  (* Processor descriptions *)
  module Mach : MACH
  (* Instruction selection *)
  val word_addressed: bool

  (* Registers available for register allocation *)
  val num_register_classes: int
  val register_class: Reg.t -> int
  val num_available_registers: int array
  val first_available_register: int array
  val register_name: int -> string
  val phys_reg: int -> Reg.t
  val rotate_registers: bool

  (* Calling conventions *)
  val loc_arguments: Reg.t array -> Reg.t array * int
  val loc_results: Reg.t array -> Reg.t array
  val loc_parameters: Reg.t array -> Reg.t array
  (* For argument number [n] split across multiple registers, the target-specific
     implementation of [loc_external_arguments] must return [regs] such that
     [regs.(n).(0)] is to hold the part of the value at the lowest address.
     (All that matters for the input to [loc_external_arguments] is the pattern
     of lengths and register types of the various supplied arrays.) *)
  val loc_external_arguments: Reg.t array array -> Reg.t array array * int
  val loc_external_results: Reg.t array -> Reg.t array
  val loc_exn_bucket: Reg.t

  (* The maximum number of arguments of an OCaml to OCaml function call for
     which it is guaranteed there will be no arguments passed on the stack.
     (Above this limit, tail call optimization may be disabled.)
     N.B. The values for this parameter in the backends currently assume
     that no unboxed floats are passed using the OCaml calling conventions.
  *)
  val max_arguments_for_tailcalls : int

  (* Maximal register pressures for pre-spilling *)
  val safe_register_pressure: Mach.operation -> int
  val max_register_pressure: Mach.operation -> int array

  (* Registers destroyed by operations *)
  val destroyed_at_oper: Mach.instruction_desc -> Reg.t array
  val destroyed_at_raise: Reg.t array

  (* Volatile registers: those that change value when read *)
  val regs_are_volatile: Reg.t array -> bool

  (* Pure operations *)
  val op_is_pure: Mach.operation -> bool

  (* Info for laying out the stack frame *)
  val num_stack_slots: int array
  val contains_calls: bool ref

  (* Calling the assembler *)
  val assemble_file: string -> string -> int

  (* Called before translating a fundecl. *)
  val init : unit -> unit
end

module type SELECTION = sig
  (* Selection of pseudo-instructions, assignment of pseudo-registers,
     sequentialization. *)
  module Mach : MACH
  val fundecl: Cmm.fundecl -> Mach.fundecl
end

module type LIVENESS = sig
  module Mach : MACH
  module Proc : PROC with module Mach = Mach
  val reset : unit -> unit
  val fundecl: Format.formatter -> Mach.fundecl -> unit
end

module type LINEARIZE = sig
  module Mach : MACH

  type label = int
  val new_label: unit -> label

  type instruction =
    { mutable desc: instruction_desc;
      mutable next: instruction;
      arg: Reg.t array;
      res: Reg.t array;
      dbg: Debuginfo.t;
      live: Reg.Set.t }

  and instruction_desc =
      Lend
    | Lop of Mach.operation
    | Lreloadretaddr
    | Lreturn
    | Llabel of label
    | Lbranch of label
    | Lcondbranch of Mach.test * label
    | Lcondbranch3 of label option * label option * label option
    | Lswitch of label array
    | Lsetuptrap of label
    | Lpushtrap
    | Lpoptrap
    | Lraise of Lambda.raise_kind

  val has_fallthrough :  instruction_desc -> bool
  val end_instr: instruction
  val instr_cons:
    instruction_desc -> Reg.t array -> Reg.t array -> instruction -> instruction
  val invert_test: Mach.test -> Mach.test

  type fundecl =
    { fun_name: string;
      fun_body: instruction;
      fun_fast: bool;
      fun_dbg : Debuginfo.t }

  val reset : unit -> unit
  val fundecl: Mach.fundecl -> fundecl
end

module type RELOAD = sig
  (* Insert load/stores for pseudoregs that got assigned to stack locations. *)
  module Mach : MACH
  val fundecl: Mach.fundecl -> Mach.fundecl * bool
end

module type SCHEDULING = sig
  (* Instruction scheduling *)
  module Linearize : LINEARIZE
  val fundecl: Linearize.fundecl -> Linearize.fundecl
end

module type CSE = sig
  (* Common subexpression elimination by value numbering over extended
     basic blocks. *)
  module Mach : MACH
  val fundecl: Mach.fundecl -> Mach.fundecl
end

module type SPILL = sig
(* Insertion of moves to suggest possible spilling / reloading points
   before register allocation. *)
module Mach : MACH
val fundecl: Mach.fundecl -> Mach.fundecl
val reset : unit -> unit
end

module type SPLIT = sig
  (* Renaming of registers at reload points to split live ranges. *)
  module Mach : MACH
  val fundecl: Mach.fundecl -> Mach.fundecl
  val reset : unit -> unit
end

module type DEADCODE = sig
  (* Dead code elimination: remove pure instructions whose results are
     not used. *)
  module Mach : MACH
  val fundecl: Mach.fundecl -> Mach.fundecl
end

module type INTERF = sig
  (* Construction of the interference graph.
     Annotate pseudoregs with interference lists and preference lists. *)
  module Mach : MACH
  val build_graph: Mach.fundecl -> unit
end

module type COLORING = sig
  (* Register allocation by coloring of the interference graph *)
  module Proc : PROC
  val allocate_registers: unit -> unit
end

module type COMBALLOC = sig
  (* Combine heap allocations occurring in the same basic block *)
  module Mach : MACH
  val fundecl: Mach.fundecl -> Mach.fundecl
end

module type EMIT = sig
  (* Generation of assembly code *)
  module Linearize : LINEARIZE
  val fundecl: Linearize.fundecl -> unit
  val data: Cmm.data_item list -> unit
  val begin_assembly: unit -> unit
  val end_assembly: unit -> unit
end

module type BACKEND = sig
  module Arch : ARCH
  module Proc : PROC
  module Mach : MACH with module Arch = Arch
  module Printmach : PRINTMACH with module Mach = Mach
  module CSE : CSE with module Mach = Mach
  module Linearize : LINEARIZE with module Mach = Mach
  module Selection : SELECTION with module Mach = Mach
  module Liveness : LIVENESS with module Mach = Mach
  module Reload : RELOAD with module Mach = Mach
  module Spill : SPILL with module Mach = Mach
  module Split : SPLIT with module Mach = Mach
  module Deadcode : DEADCODE with module Mach = Mach
  module Interf : INTERF with module Mach = Mach
  module Coloring : COLORING with module Proc = Proc
  module Comballoc : COMBALLOC with module Mach = Mach
  module Scheduling : SCHEDULING with module Linearize = Linearize
  module Emit : EMIT with module Linearize = Linearize
end
