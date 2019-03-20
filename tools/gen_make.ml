let max_length l =
  List.fold_left (fun acc s -> max acc (String.length s)) 0 l

let () =
  let prefix, modules =
    let args = Array.to_list Sys.argv |> List.tl in
    List.hd args, List.tl args |> List.map Filename.chop_extension
  in
  List.iter (fun module_ ->
      Printf.printf "compilerlibs/%s__%s.ml: %s.ml\n" prefix (Filename.basename module_) module_;
      Printf.printf "\t(echo \"# 1 \\\"%s.ml\\\"\"; cat $<) > $@\n" module_;
      Printf.printf "compilerlibs/%s__%s.mli: %s.mli\n" prefix (Filename.basename module_) module_;
      Printf.printf "\t(echo \"# 1 \\\"%s.mli\\\"\"; cat $<) > $@\n" module_;
      Printf.printf "compilerlibs/unprefixed/%s.ml: %s.ml\n" (Filename.basename module_) module_;
      Printf.printf "\techo \"[@@@ocaml.deprecated \\\"Use %s.%s instead.\\\"]\" > $@\n"
        (String.capitalize_ascii prefix) (String.capitalize_ascii (Filename.basename module_));
      Printf.printf "\techo \"include %s.%s\" >> $@\n"
        (String.capitalize_ascii prefix) (String.capitalize_ascii (Filename.basename module_))
    ) modules;
  Printf.printf "compilerlibs/%s.ml: Makefile\n" prefix;
  let modules = List.map Filename.basename modules |> List.sort Stdlib.compare in
  let len = max_length modules in
  List.iteri (fun i module_ ->
      Printf.printf "\techo \"module %-*s = %s__%s\" %s $@\n"
        len (String.capitalize_ascii module_)
        (String.capitalize_ascii prefix) module_ (if i = 0 then ">" else ">>")
    ) modules;
  Printf.printf "beforedepend:: compilerlibs/%s.ml" prefix;
  List.iter (fun module_ ->
      Printf.printf " \\\n\tcompilerlibs/%s__%s.ml" prefix module_;
      Printf.printf " \\\n\tcompilerlibs/%s__%s.mli" prefix module_;
      Printf.printf " \\\n\tcompilerlibs/unprefixed/%s.ml" module_
    ) modules;
  Printf.printf "\n"
