let modules_without_implementation =
  [
    "parsing/asttypes";
    "parsing/parsetree";
    "typing/annot";
    "typing/outcometree";
    "bytecomp/cmo_format";
    "middle_end/backend_intf";
    "middle_end/simplify_boxed_integer_ops_intf";
    "middle_end/inlining_decision_intf";
    "asmcomp/cmx_format";
    "asmcomp/cmxs_format";
    "asmcomp/x86_ast";
  ]

let modules_without_interface =
  [
    "asmcomp/CSE";
    "asmcomp/arch";
    "asmcomp/branch_relaation_intf";
    "asmcomp/debug/compute_ranges_intf";
  ]

let has_implementation s =
  not (List.mem s modules_without_implementation)

let has_interface s =
  not (List.mem s modules_without_interface)

let utils =
  [
    "utils/config";
    "utils/build_path_prefix_map";
    "utils/misc";
    "utils/identifiable";
    "utils/numbers";
    "utils/arg_helper";
    "utils/clflags";
    "utils/profile";
    "utils/load_path";
    "utils/terminfo";
    "utils/ccomp";
    "utils/warnings";
    "utils/consistbl";
    "utils/strongly_connected_components";
    "utils/targetint";
  ]

let parsing =
  [
    "parsing/location";
    "parsing/longident";
    "parsing/asttypes";
    "parsing/parsetree";
    "parsing/docstrings";
    "parsing/syntaxerr";
    "parsing/ast_helper";
    "parsing/pprintast";
    "parsing/camlinternalMenhirLib";
    "parsing/parser";
    "parsing/lexer";
    "parsing/parse";
    "parsing/printast";
    "parsing/ast_mapper";
    "parsing/ast_iterator";
    "parsing/attr_helper";
    "parsing/builtin_attributes";
    "parsing/ast_invariants";
    "parsing/depend";
  ]

let typing =
  [
    "typing/ident";
    "typing/path";
    "typing/outcometree";
    "typing/primitive";
    "typing/types";
    "typing/btype";
    "typing/oprint";
    "typing/subst";
    "typing/predef";
    "typing/datarepr";
    "typing/cmi_format";
    "typing/persistent_env";
    "typing/env";
    "typing/typedtree";
    "typing/printtyped";
    "typing/ctype";
    "typing/printtyp";
    "typing/includeclass";
    "typing/mtype";
    "typing/envaux";
    "typing/includecore";
    "typing/typedtreeIter";
    "typing/tast_mapper";
    "typing/cmt_format";
    "typing/untypeast";
    "typing/includemod";
    "typing/typetexp";
    "typing/printpat";
    "typing/parmatch";
    "typing/annot";
    "typing/stypes";
    "typing/typedecl_properties";
    "typing/typedecl_variance";
    "typing/typedecl_unboxed";
    "typing/typedecl_immediacy";
    "typing/typedecl";
    "typing/typeopt";
    "typing/rec_check";
    "typing/typecore";
    "typing/typeclass";
    "typing/typemod";
  ]

let comp =
  [
    "bytecomp/lambda";
    "bytecomp/printlambda";
    "bytecomp/switch";
    "bytecomp/matching";
    "bytecomp/translobj";
    "bytecomp/translattribute";
    "bytecomp/translprim";
    "bytecomp/translcore";
    "bytecomp/translclass";
    "bytecomp/translmod";
    "bytecomp/simplif";
    "bytecomp/runtimedef";
    "bytecomp/instruct";
    "bytecomp/meta";
    "bytecomp/opcodes";
    "bytecomp/bytesections";
    "bytecomp/dll";
    "bytecomp/cmo_format";
    "bytecomp/symtable";
    "driver/pparse";
    "driver/main_args";
    "driver/compenv";
    "driver/compmisc";
    "driver/makedepend";
    "driver/compile_common";
  ]

let common = utils @ parsing @ typing @ comp

let bytecomp =
  [
    "bytecomp/bytegen";
    "bytecomp/printinstr";
    "bytecomp/emitcode";
    "bytecomp/bytelink";
    "bytecomp/bytelibrarian";
    "bytecomp/bytepackager";
    "driver/errors";
    "driver/compile";
  ]

let max_length l =
  List.fold_left (fun acc s -> max acc (String.length s)) 0 l

let build_lib prefix modules =
  List.iter (fun module_ ->
      if has_implementation module_ then begin
        Printf.printf "%s/%s__%s.cmo: %s.ml\n"
          (Filename.dirname module_) prefix (Filename.basename module_) module_;
        Printf.printf "\t$(CAMLC) $(COMPFLAGS) -o $@ -c $<\n";
        Printf.printf "%s/%s__%s.cmx: %s.ml\n"
          (Filename.dirname module_) prefix (Filename.basename module_) module_;
        Printf.printf "\t$(CAMLOPT) $(COMPFLAGS) -o $@ -c $<\n";
      end;
      if has_interface module_ then begin
        Printf.printf "%s/%s__%s.cmi: %s.mli\n"
          (Filename.dirname module_) prefix (Filename.basename module_) module_;
        Printf.printf "\t$(CAMLC) $(COMPFLAGS) -o $@ -c $<\n"
      end
      (* Printf.printf "compilerlibs/unprefixed/%s.ml: %s.ml\n" (Filename.basename module_) module_; *)
      (* Printf.printf "\techo \"[@@@ocaml.deprecated \\\"Use %s.%s instead.\\\"]\" > $@\n" *)
      (*   (String.capitalize_ascii prefix) (String.capitalize_ascii (Filename.basename module_)); *)
      (* Printf.printf "\techo \"include %s.%s\" >> $@\n" *)
      (*   (String.capitalize_ascii prefix) (String.capitalize_ascii (Filename.basename module_)) *)
    ) modules;
  Printf.printf "compilerlibs/%s.ml: compilerlibs/gen.ml\n" prefix;
  let modules = List.map Filename.basename modules |> List.sort Stdlib.compare in
  let len = max_length modules in
  List.iteri (fun i module_ ->
      Printf.printf "\techo \"module %-*s = %s__%s\" %s $@\n"
        len (String.capitalize_ascii module_)
        (String.capitalize_ascii prefix) module_ (if i = 0 then ">" else ">>")
    ) modules;
  (* List.iter (fun module_ -> *)
  (*     Printf.printf " \\\n\tcompilerlibs/unprefixed/%s.ml" module_ *)
  (*   ) modules; *)
  Printf.printf "\n"

let generate_makefile () =
  build_lib "ocamlcommon" common;
  build_lib "ocamlbytecomp" bytecomp

let postprocess_depend () =
  let rec loop () =
    let s = read_line () in
    prerr_endline s;
    let rec scan start i =
      match s.[i] with
      | ' ' as c when start < 0 ->
          print_char c;
          scan start (i + 1)
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '/' | '.' ->
          scan (if start < 0 then i else start) (i + 1)
      | _ ->
          let filename = String.sub s start (i - start) in
          let dirname, basename = Filename.dirname filename, Filename.basename filename in
          if List.mem (Filename.remove_extension filename) common then begin
            Printf.printf "%s/%s%s" dirname "ocamlcommon__" basename;
            print_endline (String.sub s i (String.length s - i))
          end else
            print_endline s
    in
    scan (-1) 0;
    loop ()
  in
  try loop () with End_of_file -> ()

let spec =
  [
    "-generate-makefile", Arg.Unit generate_makefile, " Generate Makefile";
    "-postprocess-depend", Arg.Unit postprocess_depend, " Post-process .depend";
  ]

let () =
  Arg.parse (Arg.align spec) ignore "Build tool"
