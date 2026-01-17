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

(* Command-line parameters *)

let s_ref = Local_store.s_ref

module Int_arg_helper = Arg_helper.Make (struct
  module Key = struct
    include Numbers.Int
    let of_string = int_of_string
  end

  module Value = struct
    include Numbers.Int
    let of_string = int_of_string
  end
end)
module Float_arg_helper = Arg_helper.Make (struct
  module Key = struct
    include Numbers.Int
    let of_string = int_of_string
  end

  module Value = struct
    include Numbers.Float
    let of_string = float_of_string
  end
end)

let objfiles = s_ref ([] : string list)         (* .cmo and .cma files *)
and ccobjs = s_ref ([] : string list)           (* .o, .a, .so and -cclib -lxxx *)
and dllibs = s_ref ([] : (suffixed:bool * string) list)
                                              (* .so, -dllib -lxxx and
                                                 -dllib-suffixed -lxxx *)

let cmi_file = s_ref None

let compile_only = s_ref false            (* -c *)
and output_name = s_ref (None : string option) (* -o *)
and include_dirs = s_ref ([] : string list) (* -I *)
and hidden_include_dirs = s_ref ([] : string list) (* -H *)
and standard_library_default = s_ref None (* -set-runtime-default *)
and no_std_include = s_ref false          (* -nostdlib *)
and no_cwd = s_ref false                  (* -nocwd *)
and print_types = s_ref false             (* -i *)
and print_variance = s_ref false          (* -i-variance *)
and make_archive = s_ref false            (* -a *)
and debug = s_ref false                   (* -g *)
and debug_full = s_ref false              (* For full DWARF support *)
and unsafe = s_ref false                  (* -unsafe *)
and use_linscan = s_ref false             (* -linscan *)
and link_everything = s_ref false         (* -linkall *)
and custom_runtime = s_ref false          (* -custom *)
and no_check_prims = s_ref false          (* -no-check-prims *)
and bytecode_compatible_32 = s_ref false  (* -compat-32 *)
and output_c_object = s_ref false         (* -output-obj *)
and output_complete_object = s_ref false  (* -output-complete-obj *)
and output_complete_executable = s_ref false  (* -output-complete-exe *)
and all_ccopts = s_ref ([] : string list)     (* -ccopt *)
and classic = s_ref false                 (* -nolabels *)
and nopervasives = s_ref false            (* -nopervasives *)
and match_context_rows = s_ref 32         (* -match-context-rows *)
and safer_matching = s_ref false          (* -safer-matching *)
and preprocessor = s_ref (None : string option) (* -pp *)
and all_ppx = s_ref ([] : string list)        (* -ppx *)
let absname = s_ref false                 (* -absname *)
let annotations = s_ref false             (* -annot *)
let binary_annotations = s_ref false      (* -bin-annot *)
let store_occurrences = s_ref false       (* -bin-annot-occurrences *)
and use_threads = s_ref false             (* -thread *)
and noassert = s_ref false                (* -noassert *)
and verbose = s_ref false                 (* -verbose *)
and noversion = s_ref false               (* -no-version *)
and noprompt = s_ref false                (* -noprompt *)
and nopromptcont = s_ref false            (* -nopromptcont *)
and init_file = s_ref (None : string option)   (* -init *)
and noinit = s_ref false                  (* -noinit *)
and open_modules = s_ref []               (* -open *)
and use_prims = s_ref ""                  (* -use-prims ... *)
and use_runtime = s_ref ""                (* -use-runtime ... *)
and target_bindir =                       (* -launch-method ... *)
  s_ref Config.target_bindir
and launch_method =
  s_ref Config.launch_method
and search_method =                       (* -search-method ... *)
  s_ref Config.search_method
and plugin = s_ref false                  (* -plugin ... *)
and principal = s_ref false               (* -principal *)
and real_paths = s_ref true               (* -short-paths *)
and recursive_types = s_ref false         (* -rectypes *)
and strict_sequence = s_ref false         (* -strict-sequence *)
and strict_formats = s_ref true           (* -strict-formats *)
and applicative_functors = s_ref true     (* -no-app-funct *)
and make_runtime = s_ref false            (* -make-runtime *)
and c_compiler = s_ref (None: string option) (* -cc *)
and no_auto_link = s_ref false            (* -noautolink *)
and dllpaths = s_ref ([] : string list)   (* -dllpath *)
and make_package = s_ref false            (* -pack *)
and for_package = s_ref (None: string option) (* -for-pack *)
and error_size = s_ref 500                (* -error-size *)
and float_const_prop = s_ref true         (* -no-float-const-prop *)
and no_alias_deps = s_ref false           (* -no-alias-deps *)
let unique_ids = s_ref true               (* -d(no-)unique-ids *)
let canonical_ids = s_ref false           (* -d(no-)canonical-ids *)
let locations = s_ref true                (* -d(no-)locations *)
let dump_source = s_ref false             (* -dsource *)
let dump_parsetree = s_ref false          (* -dparsetree *)
and dump_typedtree = s_ref false          (* -dtypedtree *)
and dump_shape = s_ref false              (* -dshape *)
and dump_matchcomp = s_ref false          (* -dmatchcomp *)
and dump_rawlambda = s_ref false          (* -drawlambda *)
and dump_lambda = s_ref false             (* -dlambda *)
and dump_rawclambda = s_ref false         (* -drawclambda *)
and dump_clambda = s_ref false            (* -dclambda *)
and dump_rawflambda = s_ref false            (* -drawflambda *)
and dump_flambda = s_ref false            (* -dflambda *)
and dump_flambda_let = s_ref (None : int option) (* -dflambda-let=... *)
and dump_flambda_verbose = s_ref false    (* -dflambda-verbose *)
and dump_instr = s_ref false              (* -dinstr *)
and keep_camlprimc_file = s_ref false     (* -dcamlprimc *)

let keyword_edition: string option ref = s_ref None

let keep_asm_file = s_ref false         (* -S *)
let optimize_for_speed = s_ref true     (* -compact *)
and opaque = s_ref false                (* -opaque *)

and dump_cmm = s_ref false              (* -dcmm *)
let dump_selection = s_ref false        (* -dsel *)
let dump_combine = s_ref false          (* -dcombine *)
let dump_cse = s_ref false              (* -dcse *)
let dump_live = s_ref false             (* -dlive *)
let dump_spill = s_ref false            (* -dspill *)
let dump_split = s_ref false            (* -dsplit *)
let dump_interf = s_ref false           (* -dinterf *)
let dump_prefer = s_ref false           (* -dprefer *)
let dump_interval = s_ref false         (* -dinterval *)
let dump_regalloc = s_ref false         (* -dalloc *)
let dump_reload = s_ref false           (* -dreload *)
let dump_scheduling = s_ref false       (* -dscheduling *)
let dump_linear = s_ref false           (* -dlinear *)
let keep_startup_file = s_ref false     (* -dstartup *)
let profile_columns : Profile.column list ref = s_ref [] (* -dprofile/-dtimings *)

let native_code = s_ref false           (* set to true under ocamlopt *)

let force_slash = s_ref false           (* for ocamldep *)
let clambda_checks = s_ref false        (* -clambda-checks *)
let cmm_invariants =
  s_ref Config.with_cmm_invariants      (* -dcmm-invariants *)

let parsetree_ghost_loc_invariant = s_ref false
  (* -dparsetree-ghost-loc-invariant *)

let flambda_invariant_checks =
  s_ref Config.with_flambda_invariants  (* -flambda-(no-)invariants *)

let dont_write_files = s_ref false      (* set to true under ocamldoc *)

let insn_sched_default = true
let insn_sched = s_ref insn_sched_default (* -[no-]insn-sched *)

let std_include_flag prefix =
  if !no_std_include then ""
  else (prefix ^ (Filename.quote Config.standard_library))

let std_include_dir () =
  if !no_std_include then [] else [Config.standard_library]

let shared = s_ref false (* -shared *)
let dlcode = s_ref true (* not -nodynlink *)

let pic_code = s_ref (match Config.architecture with (* -fPIC *)
                     | "amd64" | "s390x" -> true
                     | _                 -> false)

let runtime_variant = s_ref ""

let with_runtime = s_ref true         (* -with-runtime *)

let keep_docs = s_ref false            (* -keep-docs *)
let keep_locs = s_ref true             (* -keep-locs *)
let server_mode = Misc.Server.enable   (* -server *)

let classic_inlining = s_ref false     (* -Oclassic *)
let inlining_report = s_ref false      (* -inlining-report *)

let afl_instrument = s_ref Config.afl_instrument (* -afl-instrument *)
let afl_inst_ratio = s_ref 100         (* -afl-inst-ratio *)

let function_sections = s_ref false    (* -function-sections *)

let simplify_rounds = s_ref None      (* -rounds *)
let default_simplify_rounds = s_ref 1 (* -rounds *)
let rounds () =
  match !simplify_rounds with
  | None -> !default_simplify_rounds
  | Some r -> r

let default_inline_threshold = if Config.flambda then 10. else 10. /. 8.
let inline_toplevel_multiplier = 16
let default_inline_toplevel_threshold =
  int_of_float ((float inline_toplevel_multiplier) *. default_inline_threshold)
let default_inline_call_cost = 5
let default_inline_alloc_cost = 7
let default_inline_prim_cost = 3
let default_inline_branch_cost = 5
let default_inline_indirect_cost = 4
let default_inline_branch_factor = 0.1
let default_inline_lifting_benefit = 1300
let default_inline_max_unroll = 0
let default_inline_max_depth = 1

let inline_threshold = s_ref (Float_arg_helper.default default_inline_threshold)
let inline_toplevel_threshold =
  s_ref (Int_arg_helper.default default_inline_toplevel_threshold)
let inline_call_cost = s_ref (Int_arg_helper.default default_inline_call_cost)
let inline_alloc_cost = s_ref (Int_arg_helper.default default_inline_alloc_cost)
let inline_prim_cost = s_ref (Int_arg_helper.default default_inline_prim_cost)
let inline_branch_cost =
  s_ref (Int_arg_helper.default default_inline_branch_cost)
let inline_indirect_cost =
  s_ref (Int_arg_helper.default default_inline_indirect_cost)
let inline_branch_factor =
  s_ref (Float_arg_helper.default default_inline_branch_factor)
let inline_lifting_benefit =
  s_ref (Int_arg_helper.default default_inline_lifting_benefit)
let inline_max_unroll =
  s_ref (Int_arg_helper.default default_inline_max_unroll)
let inline_max_depth =
  s_ref (Int_arg_helper.default default_inline_max_depth)


let unbox_specialised_args = s_ref true   (* -no-unbox-specialised-args *)
let unbox_free_vars_of_closures = s_ref true
let unbox_closures = s_ref false          (* -unbox-closures *)
let default_unbox_closures_factor = 10
let unbox_closures_factor =
  s_ref default_unbox_closures_factor    (* -unbox-closures-factor *)
let remove_unused_arguments = s_ref false (* -remove-unused-arguments *)

type inlining_arguments = {
  inline_call_cost : int option;
  inline_alloc_cost : int option;
  inline_prim_cost : int option;
  inline_branch_cost : int option;
  inline_indirect_cost : int option;
  inline_lifting_benefit : int option;
  inline_branch_factor : float option;
  inline_max_depth : int option;
  inline_max_unroll : int option;
  inline_threshold : float option;
  inline_toplevel_threshold : int option;
}

let set_int_arg round (arg:Int_arg_helper.parsed ref) default value =
  let value : int =
    match value with
    | None -> default
    | Some value -> value
  in
  match round with
  | None ->
    arg := Int_arg_helper.set_base_default value
             (Int_arg_helper.reset_base_overrides !arg)
  | Some round ->
    arg := Int_arg_helper.add_base_override round value !arg

let set_float_arg round (arg:Float_arg_helper.parsed ref) default value =
  let value =
    match value with
    | None -> default
    | Some value -> value
  in
  match round with
  | None ->
    arg := Float_arg_helper.set_base_default value
             (Float_arg_helper.reset_base_overrides !arg)
  | Some round ->
    arg := Float_arg_helper.add_base_override round value !arg

let use_inlining_arguments_set ?round (arg:inlining_arguments) =
  let set_int = set_int_arg round in
  let set_float = set_float_arg round in
  set_int inline_call_cost default_inline_call_cost arg.inline_call_cost;
  set_int inline_alloc_cost default_inline_alloc_cost arg.inline_alloc_cost;
  set_int inline_prim_cost default_inline_prim_cost arg.inline_prim_cost;
  set_int inline_branch_cost
    default_inline_branch_cost arg.inline_branch_cost;
  set_int inline_indirect_cost
    default_inline_indirect_cost arg.inline_indirect_cost;
  set_int inline_lifting_benefit
    default_inline_lifting_benefit arg.inline_lifting_benefit;
  set_float inline_branch_factor
    default_inline_branch_factor arg.inline_branch_factor;
  set_int inline_max_depth
    default_inline_max_depth arg.inline_max_depth;
  set_int inline_max_unroll
    default_inline_max_unroll arg.inline_max_unroll;
  set_float inline_threshold
    default_inline_threshold arg.inline_threshold;
  set_int inline_toplevel_threshold
    default_inline_toplevel_threshold arg.inline_toplevel_threshold

(* o1 is the default *)
let o1_arguments = {
  inline_call_cost = None;
  inline_alloc_cost = None;
  inline_prim_cost = None;
  inline_branch_cost = None;
  inline_indirect_cost = None;
  inline_lifting_benefit = None;
  inline_branch_factor = None;
  inline_max_depth = None;
  inline_max_unroll = None;
  inline_threshold = None;
  inline_toplevel_threshold = None;
}

let classic_arguments = {
  inline_call_cost = None;
  inline_alloc_cost = None;
  inline_prim_cost = None;
  inline_branch_cost = None;
  inline_indirect_cost = None;
  inline_lifting_benefit = None;
  inline_branch_factor = None;
  inline_max_depth = None;
  inline_max_unroll = None;
  (* [inline_threshold] matches the current compiler's default.
     Note that this particular fraction can be expressed exactly in
     floating point. *)
  inline_threshold = Some (10. /. 8.);
  (* [inline_toplevel_threshold] is not used in classic mode. *)
  inline_toplevel_threshold = Some 1;
}

let o2_arguments = {
  inline_call_cost = Some (2 * default_inline_call_cost);
  inline_alloc_cost = Some (2 * default_inline_alloc_cost);
  inline_prim_cost = Some (2 * default_inline_prim_cost);
  inline_branch_cost = Some (2 * default_inline_branch_cost);
  inline_indirect_cost = Some (2 * default_inline_indirect_cost);
  inline_lifting_benefit = None;
  inline_branch_factor = None;
  inline_max_depth = Some 2;
  inline_max_unroll = None;
  inline_threshold = Some 25.;
  inline_toplevel_threshold = Some (25 * inline_toplevel_multiplier);
}

let o3_arguments = {
  inline_call_cost = Some (3 * default_inline_call_cost);
  inline_alloc_cost = Some (3 * default_inline_alloc_cost);
  inline_prim_cost = Some (3 * default_inline_prim_cost);
  inline_branch_cost = Some (3 * default_inline_branch_cost);
  inline_indirect_cost = Some (3 * default_inline_indirect_cost);
  inline_lifting_benefit = None;
  inline_branch_factor = Some 0.;
  inline_max_depth = Some 3;
  inline_max_unroll = Some 1;
  inline_threshold = Some 50.;
  inline_toplevel_threshold = Some (50 * inline_toplevel_multiplier);
}

let all_passes = s_ref []
let dumped_passes_list = s_ref []
let dumped_pass s =
  assert(List.mem s !all_passes);
  List.mem s !dumped_passes_list

let set_dumped_pass s enabled =
  if (List.mem s !all_passes) then begin
    let passes_without_s = List.filter ((<>) s) !dumped_passes_list in
    let dumped_passes =
      if enabled then
        s :: passes_without_s
      else
        passes_without_s
    in
    dumped_passes_list := dumped_passes
  end

let dump_into_file = s_ref false (* -dump-into-file *)
let dump_dir: string option ref = s_ref None (* -dump-dir *)

type 'a env_reader = {
  parse : string -> 'a option;
  print : 'a -> string;
  usage : string;
  env_var : string;
}

let color = s_ref None (* -color *)

let color_reader = {
  parse = (function
    | "auto" -> Some Misc.Color.Auto
    | "always" -> Some Misc.Color.Always
    | "never" -> Some Misc.Color.Never
    | _ -> None);
  print = (function
    | Misc.Color.Auto -> "auto"
    | Misc.Color.Always -> "always"
    | Misc.Color.Never -> "never");
  usage = "expected \"auto\", \"always\" or \"never\"";
  env_var = "OCAML_COLOR";
}

let error_style = s_ref None (* -error-style *)

let error_style_reader = {
  parse = (function
    | "contextual" -> Some Misc.Error_style.Contextual
    | "short" -> Some Misc.Error_style.Short
    | _ -> None);
  print = (function
    | Misc.Error_style.Contextual -> "contextual"
    | Misc.Error_style.Short -> "short");
  usage = "expected \"contextual\" or \"short\"";
  env_var = "OCAML_ERROR_STYLE";
}

let unboxed_types = s_ref false

(* This is used by the -save-ir-after option. *)
module Compiler_ir = struct
  type t = Linear

  let all = [
    Linear;
  ]

  let extension t =
    let ext =
    match t with
      | Linear -> "linear"
    in
    ".cmir-" ^ ext

  (** [extract_extension_with_pass filename] returns the IR whose extension
      is a prefix of the extension of [filename], and the suffix,
      which can be used to distinguish different passes on the same IR.
      For example, [extract_extension_with_pass "foo.cmir-linear123"]
      returns [Some (Linear, "123")]. *)
  let extract_extension_with_pass filename =
    let ext = Filename.extension filename in
    let ext_len = String.length ext in
    if ext_len <= 0 then None
    else begin
      let is_prefix ir =
        let s = extension ir in
        let s_len = String.length s in
        s_len <= ext_len && s = String.sub ext 0 s_len
      in
      let drop_prefix ir =
        let s = extension ir in
        let s_len = String.length s in
        String.sub ext s_len (ext_len - s_len)
      in
      let ir = List.find_opt is_prefix all in
      match ir with
      | None -> None
      | Some ir -> Some (ir, drop_prefix ir)
    end
end

(* This is used by the -stop-after option. *)
module Compiler_pass = struct
  (* If you add a new pass, the following must be updated:
     - the variable `passes` below
     - the manpages in man/ocaml{c,opt}.m
     - the manual manual/src/cmds/unified-options.etex
  *)
  type t = Parsing | Typing | Lambda | Scheduling | Emit

  let to_string = function
    | Parsing -> "parsing"
    | Typing -> "typing"
    | Lambda -> "lambda"
    | Scheduling -> "scheduling"
    | Emit -> "emit"

  let of_string = function
    | "parsing" -> Some Parsing
    | "typing" -> Some Typing
    | "lambda" -> Some Lambda
    | "scheduling" -> Some Scheduling
    | "emit" -> Some Emit
    | _ -> None

  let rank = function
    | Parsing -> 0
    | Typing -> 1
    | Lambda -> 2
    | Scheduling -> 50
    | Emit -> 60

  let passes = [
    Parsing;
    Typing;
    Lambda;
    Scheduling;
    Emit;
  ]
  let is_compilation_pass _ = true
  let is_native_only = function
    | Scheduling -> true
    | Emit -> true
    | _ -> false

  let enabled is_native t = not (is_native_only t) || is_native
  let can_save_ir_after = function
    | Scheduling -> true
    | _ -> false

  let available_pass_names ~filter ~native =
    passes
    |> List.filter (enabled native)
    |> List.filter filter
    |> List.map to_string

  let compare a b =
    compare (rank a) (rank b)

  let to_output_filename t ~prefix =
    match t with
    | Scheduling -> prefix ^ Compiler_ir.(extension Linear)
    | _ -> Misc.fatal_error "Not supported"

  let of_input_filename name =
    match Compiler_ir.extract_extension_with_pass name with
    | Some (Linear, _) -> Some Emit
    | None -> None
end

let stop_after = s_ref None (* -stop-after *)

let should_stop_after pass =
  if Compiler_pass.(rank Typing <= rank pass) && !print_types then true
  else
    match !stop_after with
    | None -> false
    | Some stop -> Compiler_pass.rank stop <= Compiler_pass.rank pass

let save_ir_after = s_ref []

let should_save_ir_after pass =
  List.mem pass !save_ir_after

let set_save_ir_after pass enabled =
  let other_passes = List.filter ((<>) pass) !save_ir_after in
  let new_passes =
    if enabled then
      pass :: other_passes
    else
      other_passes
  in
  save_ir_after := new_passes

module Dump_option = struct
  type t =
    | Source
    | Parsetree
    | Typedtree
    | Shape
    | Match_comp
    | Raw_lambda
    | Lambda
    | Instr
    | Raw_clambda
    | Clambda
    | Raw_flambda
    | Flambda
    | Cmm
    | Selection
    | Combine
    | CSE
    | Live
    | Spill
    | Split
    | Interf
    | Prefer
    | Regalloc
    | Scheduling
    | Linear
    | Interval

  let compare (op1 : t) op2 =
    Stdlib.compare op1 op2

  let to_string = function
    | Source -> "source"
    | Parsetree -> "parsetree"
    | Typedtree -> "typedtree"
    | Shape -> "shape"
    | Match_comp -> "matchcomp"
    | Raw_lambda -> "rawlambda"
    | Lambda -> "lambda"
    | Instr -> "instr"
    | Raw_clambda -> "rawclambda"
    | Clambda -> "clambda"
    | Raw_flambda -> "rawflambda"
    | Flambda -> "flambda"
    | Cmm -> "cmm"
    | Selection -> "selection"
    | Combine -> "combine"
    | CSE -> "cse"
    | Live -> "live"
    | Spill -> "spill"
    | Split -> "split"
    | Interf -> "interf"
    | Prefer -> "prefer"
    | Regalloc -> "regalloc"
    | Scheduling -> "scheduling"
    | Linear -> "linear"
    | Interval -> "interval"

  let of_string = function
    | "source" -> Some Source
    | "parsetree" -> Some Parsetree
    | "typedtree" -> Some Typedtree
    | "shape" -> Some Shape
    | "matchcomp" -> Some Match_comp
    | "rawlambda" -> Some Raw_lambda
    | "lambda" -> Some Lambda
    | "instr" -> Some Instr
    | "rawclambda" -> Some Raw_clambda
    | "clambda" -> Some Clambda
    | "rawflambda" -> Some Raw_flambda
    | "flambda" -> Some Flambda
    | "cmm" -> Some Cmm
    | "selection" -> Some Selection
    | "combine" -> Some Combine
    | "cse" -> Some CSE
    | "live" -> Some Live
    | "spill" -> Some Spill
    | "split" -> Some Split
    | "interf" -> Some Interf
    | "prefer" -> Some Prefer
    | "regalloc" -> Some Regalloc
    | "scheduling" -> Some Scheduling
    | "linear" -> Some Linear
    | "interval" -> Some Interval
    | _ -> None

  let flag = function
    | Source -> dump_source
    | Parsetree -> dump_parsetree
    | Typedtree -> dump_typedtree
    | Shape -> dump_shape
    | Match_comp -> dump_matchcomp
    | Raw_lambda -> dump_rawlambda
    | Lambda -> dump_lambda
    | Instr -> dump_instr
    | Raw_clambda -> dump_rawclambda
    | Clambda -> dump_clambda
    | Raw_flambda -> dump_rawflambda
    | Flambda -> dump_flambda
    | Cmm -> dump_cmm
    | Selection -> dump_selection
    | Combine -> dump_combine
    | CSE -> dump_cse
    | Live -> dump_live
    | Spill -> dump_spill
    | Split -> dump_split
    | Interf -> dump_interf
    | Prefer -> dump_prefer
    | Regalloc -> dump_regalloc
    | Scheduling -> dump_scheduling
    | Linear -> dump_linear
    | Interval -> dump_interval

  type middle_end =
    | Flambda
    | Any
    | Closure

  type class_ =
    | Frontend
    | Bytecode
    | Middle of middle_end
    | Backend

  let _ =
    (* no Closure-specific dump option for now, silence a warning *)
    Closure

  let classify : t -> class_ = function
    | Source
    | Parsetree
    | Typedtree
    | Shape
    | Match_comp
    | Raw_lambda
    | Lambda
      -> Frontend
    | Instr
      -> Bytecode
    | Raw_clambda
    | Clambda
      -> Middle Any
    | Raw_flambda
    | Flambda
      -> Middle Flambda
    | Cmm
    | Selection
    | Combine
    | CSE
    | Live
    | Spill
    | Split
    | Interf
    | Prefer
    | Regalloc
    | Scheduling
    | Linear
    | Interval
      -> Backend

  let available (option : t) : (unit, string) result =
    let pass = Result.ok () in
    let ( let* ) = Result.bind in
    let fail descr =
      Error (
        Printf.sprintf
          "this compiler does not support %s-specific options"
          descr
      ) in
    let guard descr cond =
      if cond then pass
      else fail descr in
    let check_bytecode = guard "bytecode" (not !native_code) in
    let check_native = guard "native" !native_code in
    let check_middle_end = function
      | Flambda -> guard "flambda" Config.flambda
      | Closure -> guard "closure" (not Config.flambda)
      | Any -> pass
    in
    match classify option with
    | Frontend ->
        pass
    | Bytecode ->
        check_bytecode
    | Middle middle_end ->
        let* () = check_native in
        check_middle_end middle_end
    | Backend ->
        check_native
end

let parse_keyword_edition s =
  let parse_version s =
  let bad_version () =
    raise (Arg.Bad "Ill-formed version in keywords flag,\n\
                    the supported format is <major>.<minor>, for example 5.2 .")
  in
  if s = "" then None else match String.split_on_char '.' s with
  | [] | [_] | _ :: _ :: _ :: _ -> bad_version ()
  | [major;minor] -> match int_of_string_opt major, int_of_string_opt minor with
    | Some major, Some minor -> Some (major,minor)
    | _ -> bad_version ()
  in
  match String.split_on_char '+' s with
  | [] -> None, []
  | [s] -> parse_version s, []
  | v :: rest -> parse_version v, rest

module String = Misc.Stdlib.String

let arg_spec = s_ref []
let arg_names = s_ref String.Map.empty

let reset_arguments () =
  arg_spec := [];
  arg_names := String.Map.empty

let add_arguments loc args =
  List.iter (function (arg_name, _, _) as arg ->
    try
      let loc2 = String.Map.find arg_name !arg_names in
      Misc.Out.eprintf
        "Warning: compiler argument %s is already defined:\n" arg_name;
      Misc.Out.eprintf "   First definition: %s\n" loc2;
      Misc.Out.eprintf "   New definition: %s\n" loc;
    with Not_found ->
      arg_spec := !arg_spec @ [ arg ];
      arg_names := String.Map.add arg_name loc !arg_names
  ) args

let create_usage_msg program =
  Printf.sprintf "Usage: %s <options> <files>\n\
    Try '%s --help' for more information." program program


let print_arguments program =
  let usage = Arg.usage_string !arg_spec (create_usage_msg program) in
  Misc.Out.print_string usage
