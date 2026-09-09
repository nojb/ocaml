(* TEST
 readonly_files = "a.ml";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 all_modules = "a.ml plain_member.ml";
 ocamlc.byte;
 compile_only = "false";
 flags = "-a";
 program = "plain.cma";
 all_modules = "a.cmo";
 ocamlc.byte;
 flags = "-a -thin";
 program = "thin.cma";
 all_modules = "plain.cma";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A plain library cannot be a member of a thin one: its own members are not
   files of their own. *)
