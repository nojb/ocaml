module type ARCH = sig
  (* Machine-specific command-line options *)
  val command_line_options: Arg.spec

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
  val print_specific_operation: (Format.formatter -> Reg.t -> unit) -> specific_operation -> Format.formatter -> _ -> unit
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
  module Arch : ARCH
  module Mach : MACH with module Arch = Arch

  type environment = (Ident.t, Reg.t array) Tbl.t

  val size_expr : environment -> Cmm.expression -> int

  class virtual selector_generic : object
    (* The following methods must or can be overridden by the processor
       description *)
    method virtual is_immediate : int -> bool
    (* Must be defined to indicate whether a constant is a suitable
       immediate operand to arithmetic instructions *)
    method virtual select_addressing :
      Cmm.memory_chunk -> Cmm.expression -> Arch.addressing_mode * Cmm.expression
    (* Must be defined to select addressing modes *)
    method is_simple_expr: Cmm.expression -> bool
    (* Can be overridden to reflect special extcalls known to be pure *)
    method select_operation :
      Cmm.operation ->
      Cmm.expression list -> Mach.operation * Cmm.expression list
    (* Can be overridden to deal with special arithmetic instructions *)
    method select_condition : Cmm.expression -> Mach.test * Cmm.expression
    (* Can be overridden to deal with special test instructions *)
    method select_store :
      bool -> Arch.addressing_mode -> Cmm.expression ->
      Mach.operation * Cmm.expression
    (* Can be overridden to deal with special store constant instructions *)
    method regs_for : Cmm.machtype -> Reg.t array
    (* Return an array of fresh registers of the given type.
       Default implementation is like Reg.createv.
       Can be overridden if float values are stored as pairs of
       integer registers. *)
    method insert_op :
      Mach.operation -> Reg.t array -> Reg.t array -> Reg.t array
    (* Can be overridden to deal with 2-address instructions
       or instructions with hardwired input/output registers *)
    method insert_op_debug :
      Mach.operation -> Debuginfo.t -> Reg.t array -> Reg.t array -> Reg.t array
    (* Can be overridden to deal with 2-address instructions
       or instructions with hardwired input/output registers *)
    method emit_extcall_args :
      environment -> Cmm.expression list -> Reg.t array * int
    (* Can be overridden to deal with stack-based calling conventions *)
    method emit_stores :
      environment -> Cmm.expression list -> Reg.t array -> unit
    (* Fill a freshly allocated block.  Can be overridden for architectures
       that do not provide Arch.offset_addressing. *)

    method mark_call : unit
    (* informs the code emitter that the current function is non-leaf:
       it may perform a (non-tail) call; by default, sets
       [Proc.contains_calls := true] *)

    method mark_tailcall : unit
    (* informs the code emitter that the current function may end with
       a tail-call; by default, does nothing *)

    method mark_c_tailcall : unit
    (* informs the code emitter that the current function may call
       a C function that never returns; by default, does nothing.

       It is unecessary to save the stack pointer in this situation
       (which is the main purpose of tracking leaf functions) but some
       architectures still need to ensure that the stack is properly
       aligned when the C function is called. This is achieved by
       overloading this method to set [Proc.contains_calls := true] *)

    method mark_instr : Mach.instruction_desc -> unit
    (* dispatches on instructions to call one of the marking function
       above; overloading this is useful if Ispecific instructions need
       marking *)

    (* The following method is the entry point and should not be overridden *)
    method emit_fundecl : Cmm.fundecl -> Mach.fundecl

    (* The following methods should not be overridden.  They cannot be
       declared "private" in the current implementation because they
       are not always applied to "self", but ideally they should be private. *)
    method extract : Mach.instruction
    method insert : Mach.instruction_desc -> Reg.t array -> Reg.t array -> unit
    method insert_debug : Mach.instruction_desc -> Debuginfo.t ->
      Reg.t array -> Reg.t array -> unit
    method insert_move : Reg.t -> Reg.t -> unit
    method insert_move_args : Reg.t array -> Reg.t array -> int -> unit
    method insert_move_results : Reg.t array -> Reg.t array -> int -> unit
    method insert_moves : Reg.t array -> Reg.t array -> unit
    method adjust_type : Reg.t -> Reg.t -> unit
    method adjust_types : Reg.t array -> Reg.t array -> unit
    method emit_expr :
      (Ident.t, Reg.t array) Tbl.t -> Cmm.expression -> Reg.t array option
    method emit_tail : (Ident.t, Reg.t array) Tbl.t -> Cmm.expression -> unit
  end

  val reset : unit -> unit
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
  module Mach : MACH

  class reload_generic : object
    method reload_operation :
      Mach.operation -> Reg.t array -> Reg.t array -> Reg.t array * Reg.t array
    method reload_test : Mach.test -> Reg.t array -> Reg.t array
    (* Can be overridden to reflect instructions that can operate
       directly on stack locations *)
    method makereg : Reg.t -> Reg.t
    (* Can be overridden to avoid creating new registers of some class
       (i.e. if all "registers" of that class are actually on stack) *)
    method fundecl : Mach.fundecl -> Mach.fundecl * bool
    (* The entry point *)
  end
end

module type SCHEDULING = sig
  module Mach : MACH
  module Linearize : LINEARIZE with module Mach = Mach

  type code_dag_node =
    { instr: Linearize.instruction;
      delay: int;
      mutable sons: (code_dag_node * int) list;
      mutable date: int;
      mutable length: int;
      mutable ancestors: int;
      mutable emitted_ancestors: int }

  class virtual scheduler_generic : object
    (* Can be overridden by processor description *)
    method virtual oper_issue_cycles : Mach.operation -> int
    (* Number of cycles needed to issue the given operation *)
    method virtual oper_latency : Mach.operation -> int
    (* Number of cycles needed to complete the given operation *)
    method reload_retaddr_issue_cycles : int
    (* Number of cycles needed to issue a Lreloadretaddr operation *)
    method reload_retaddr_latency : int
    (* Number of cycles needed to complete a Lreloadretaddr operation *)
    method oper_in_basic_block : Mach.operation -> bool
    (* Says whether the given operation terminates a basic block *)
    method is_store : Mach.operation -> bool
    (* Says whether the given operation is a memory store *)
    method is_load : Mach.operation -> bool
    (* Says whether the given operation is a memory load *)
    method is_checkbound : Mach.operation -> bool
    (* Says whether the given operation is a checkbound *)
    (* Entry point *)
    method schedule_fundecl : Linearize.fundecl -> Linearize.fundecl
  end

  val reset : unit -> unit
end

module type CSE = sig
  (* Common subexpression elimination by value numbering over extended
     basic blocks. *)

  module Mach : MACH

  type op_class =
    | Op_pure     (* pure, produce one result *)
    | Op_checkbound     (* checkbound-style: no result, can raise an exn *)
    | Op_load           (* memory load *)
    | Op_store of bool  (* memory store, false = init, true = assign *)
    | Op_other   (* anything else that does not allocate nor store in memory *)

  class cse_generic : object
    (* The following methods can be overriden to handle processor-specific
       operations. *)

    method class_of_operation: Mach.operation -> op_class

    method is_cheap_operation: Mach.operation -> bool
    (* Operations that are so cheap that it isn't worth factoring them. *)

    (* The following method is the entry point and should not be overridden *)
    method fundecl: Mach.fundecl -> Mach.fundecl

  end
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

module type BACKEND = sig
  module Arch : ARCH
  module Proc : PROC
  module Mach : MACH with module Arch = Arch
  module CSE : CSE with module Mach = Mach
  module Linearize : LINEARIZE with module Mach = Mach
  module Selection : SELECTION with module Arch = Arch and module Mach = Mach
  module Liveness : LIVENESS with module Mach = Mach
  module Reload : RELOAD with module Mach = Mach
  module Spill : SPILL with module Mach = Mach
  module Split : SPLIT with module Mach = Mach
  module Deadcode : DEADCODE with module Mach = Mach
  module Interf : INTERF with module Mach = Mach
  module Coloring : COLORING with module Proc = Proc
  module Comballoc : COMBALLOC with module Mach = Mach
  module Scheduling : SCHEDULING with module Mach = Mach and module Linearize = Linearize
end
