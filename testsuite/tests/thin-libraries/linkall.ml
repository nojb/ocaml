(* TEST
 readonly_files = "a.ml b.ml unused.ml";
 {
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   all_modules = "a.ml b.ml unused.ml linkall.ml";
   ocamlc.byte;
   compile_only = "false";
   flags = "-a -thin -linkall";
   program = "linkalllib.cma";
   all_modules = "a.cmo b.cmo unused.cmo";
   ocamlc.byte;
   flags = "";
   program = "${test_build_directory}/linkall.byte";
   all_modules = "linkalllib.cma linkall.cmo";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   compile_only = "true";
   all_modules = "a.ml b.ml unused.ml linkall.ml";
   ocamlopt.byte;
   compile_only = "false";
   flags = "-a -thin -linkall";
   program = "linkalllib.cmxa";
   all_modules = "a.cmx b.cmx unused.cmx";
   ocamlopt.byte;
   flags = "";
   program = "${test_build_directory}/linkall.native";
   all_modules = "linkalllib.cmxa linkall.cmx";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* -linkall at archive creation time is recorded in the thin library, so
   [Unused] is linked in even though nothing refers to it. *)

let () = B.go ()
