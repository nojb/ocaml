(* TEST
 readonly_files = "a.ml b.ml unused.ml";
 {
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   all_modules = "a.ml b.ml unused.ml thin.ml";
   ocamlc.byte;
   compile_only = "false";
   flags = "-a -thin";
   program = "thinlib.cma";
   all_modules = "a.cmo b.cmo unused.cmo";
   ocamlc.byte;
   flags = "";
   program = "${test_build_directory}/thin.byte";
   all_modules = "thinlib.cma thin.cmo";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   compile_only = "true";
   all_modules = "a.ml b.ml unused.ml thin.ml";
   ocamlopt.byte;
   compile_only = "false";
   flags = "-a -thin";
   program = "thinlib.cmxa";
   all_modules = "a.cmx b.cmx unused.cmx";
   ocamlopt.byte;
   flags = "";
   program = "${test_build_directory}/thin.native";
   all_modules = "thinlib.cmxa thin.cmx";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

let () = B.go ()
