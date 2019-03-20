let max_length l =
  List.fold_left (fun acc s -> max acc (String.length s)) 0 l

let () =
  let modules = Array.to_list Sys.argv |> List.tl |> List.map Filename.chop_extension in
  List.iter (fun module_ ->
      Printf.printf "compilerlibs/ocamlcommon__%s.ml: %s.ml\n" (Filename.basename module_) module_;
      Printf.printf "\t(echo \"# 1 \\\"%s.ml\\\"\"; cat $<) > $@\n" module_;
      Printf.printf "compilerlibs/ocamlcommon__%s.mli: %s.mli\n" (Filename.basename module_) module_;
      Printf.printf "\t(echo \"# 1 \\\"%s.mli\\\"\"; cat $<) > $@\n" module_;
      Printf.printf "compilerlibs/unprefixed/%s.ml: %s.ml\n" (Filename.basename module_) module_;
      Printf.printf "\techo \"[@@@ocaml.deprecated \\\"Use Ocamlcommon.%s instead.\\\"]\" > $@\n"
        (String.capitalize_ascii (Filename.basename module_));
      Printf.printf "\techo \"include Ocamlcommon.%s\" >> $@\n"
        (String.capitalize_ascii (Filename.basename module_))
    ) modules;
  Printf.printf "compilerlibs/ocamlcommon.ml: Makefile\n";
  let modules = List.map Filename.basename modules |> List.sort Stdlib.compare in
  let len = max_length modules in
  List.iteri (fun i module_ ->
      Printf.printf "\techo \"module %-*s = Ocamlcommon__%s\" %s $@\n" len (String.capitalize_ascii module_) module_
        (if i = 0 then ">" else ">>")
    ) modules;
  Printf.printf "beforedepend:: compilerlibs/ocamlcommon.ml";
  List.iter (fun module_ ->
      Printf.printf " \\\n\tcompilerlibs/ocamlcommon__%s.ml" module_;
      Printf.printf " \\\n\tcompilerlibs/ocamlcommon__%s.mli" module_;
      Printf.printf " \\\n\tcompilerlibs/unprefixed/%s.ml" module_
    ) modules;
  Printf.printf "\n"
