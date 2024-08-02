open Objinfo
open Misc

let err kind =
  exit_errf
    "This is the bytecode-only version of this tool: the object \
     file type %S is not supported." (Magic_number.human_name_of_kind kind)

let _ =
  main ~cmx:(fun config -> err (Cmx config)) ~cmxa:(fun config -> err (Cmxa config)) []
