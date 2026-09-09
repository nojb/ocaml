(* TEST
 readonly_files = "a.ml b.ml unused.ml";
 {
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   all_modules = "a.ml b.ml unused.ml nested.ml";
   ocamlc.byte;
   compile_only = "false";
   flags = "-a -thin";
   program = "inner.cma";
   all_modules = "a.cmo b.cmo";
   ocamlc.byte;
   program = "outer.cma";
   all_modules = "inner.cma unused.cmo";
   ocamlc.byte;
   flags = "";
   program = "${test_build_directory}/nested.byte";
   all_modules = "outer.cma nested.cmo";
   ocamlc.byte;
   run;
   check-program-output;
 }
*)

(* The members of a thin library given to [-a -thin] are spliced into the
   new library, so [outer.cma] lists a.cmo, b.cmo and unused.cmo. *)

let () = B.go ()
