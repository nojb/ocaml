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

(* Build libraries of .cmx files *)

open Misc
open Config
open Cmx_format

type error =
    File_not_found of string
  | Archiver_error of string
  | Link_error of Linkdeps.error

exception Error of error

let default_ui_export_info =
  if Config.flambda then
    Cmx_format.Flambda Export_info.empty
  else
    Cmx_format.Clambda Clambda.Value_unknown

let read_info name =
  let filename =
    try
      Load_path.find name
    with Not_found ->
      raise(Error(File_not_found name)) in
  let (info, crc) = Compilenv.read_unit_info filename in
  info.ui_force_link <- info.ui_force_link || !Clflags.link_everything;
  (* There is no need to keep the approximation in the .cmxa file,
     since the compiler will go looking directly for .cmx files.
     The linker, which is the only one that reads .cmxa files, does not
     need the approximation. *)
  info.ui_export_info <- default_ui_export_info;
  filename, (info, crc)

let object_file_of_unit file_name =
  Filename.chop_suffix file_name ".cmx" ^ ext_obj

let check_units units =
  List.iter
    (fun (file_name, (unit, crc)) ->
       Asmlink.check_consistency file_name unit crc)
    units;
  let ldeps = Linkdeps.create ~complete:false in
  List.iter
    (fun (filename, (unit, _crc)) ->
       Linkdeps.add ldeps
         ~filename ~compunit:unit.ui_name
         ~provides:[unit.ui_name]
         ~requires:(List.map fst unit.ui_imports_cmx))
    (List.rev units);
  match Linkdeps.check ldeps with
  | None -> ()
  | Some e -> raise (Error (Link_error e))

let create_plain_archive file_list lib_name =
  let archive_name = Filename.remove_extension lib_name ^ ext_lib in
  let outchan = open_out_bin lib_name in
  Misc.try_finally
    ~always:(fun () -> close_out outchan)
    ~exceptionally:(fun () -> remove_file lib_name; remove_file archive_name)
    (fun () ->
       output_string outchan cmxa_magic_number;
       let units = List.map read_info file_list in
       let objfiles =
         List.map (fun (file_name, _) -> object_file_of_unit file_name) units in
       check_units units;
       let infos =
         { lib_units = List.map snd units;
           lib_ccobjs = !Clflags.ccobjs;
           lib_ccopts = !Clflags.all_ccopts } in
       output_value outchan infos;
       if Ccomp.create_archive archive_name objfiles <> 0
       then raise(Error(Archiver_error archive_name));
    )

(* A thin library records where its members are to be found instead of a copy
   of their infos, and has no matching .a library: the linker passes the
   object files of the members it selects to the C linker itself. *)

let create_thin_archive file_list lib_name =
  let units = List.map read_info file_list in
  check_units units;
  List.iter
    (fun (file_name, _) ->
       let obj = object_file_of_unit file_name in
       if not (Sys.file_exists obj) then raise(Error(File_not_found obj)))
    units;
  let dir = Filename.dirname lib_name in
  let infos =
    { tlib_units =
        List.map
          (fun (file_name, _) ->
             { tu_path = Misc.path_relative_to ~dir file_name;
               tu_force_link = !Clflags.link_everything })
          units;
      tlib_ccobjs = !Clflags.ccobjs;
      tlib_ccopts = !Clflags.all_ccopts } in
  let outchan = open_out_bin lib_name in
  Misc.try_finally
    ~always:(fun () -> close_out outchan)
    ~exceptionally:(fun () -> remove_file lib_name)
    (fun () ->
       output_string outchan cmxa_thin_magic_number;
       output_value outchan infos)

let create_archive file_list lib_name =
  if !Clflags.thin_archive then create_thin_archive file_list lib_name
  else create_plain_archive file_list lib_name

module Style = Misc.Style
open Format_doc

let report_error_doc ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %a" Style.inline_code name
  | Archiver_error name ->
      fprintf ppf "Error while creating the library %a" Style.inline_code name
  | Link_error e ->
      Linkdeps.report_error_doc ~print_filename:Location.Doc.filename ppf e

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error_doc err)
      | _ -> None
    )

let report_error = Format_doc.compat report_error_doc
