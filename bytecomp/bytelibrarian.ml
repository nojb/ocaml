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

(* Build libraries of .cmo files *)

open Misc
open Config
open Cmo_format

type error =
    File_not_found of string
  | Not_an_object_file of string
  | Full_archive_member of string
  | Link_error of Linkdeps.error

exception Error of error

(* Copy a compilation unit from a .cmo or .cma into the archive *)
let copy_compunit ic oc compunit =
  seek_in ic compunit.cu_pos;
  compunit.cu_pos <- pos_out oc;
  compunit.cu_force_link <- compunit.cu_force_link || !Clflags.link_everything;
  copy_file_chunk ic oc compunit.cu_codesize;
  if compunit.cu_debug > 0 then begin
    seek_in ic compunit.cu_debug;
    compunit.cu_debug <- pos_out oc;
    copy_file_chunk ic oc compunit.cu_debugsize
  end;
  if compunit.cu_hint > 0 then begin
    seek_in ic compunit.cu_hint;
    compunit.cu_hint <- pos_out oc;
    copy_file_chunk ic oc compunit.cu_hintsize
  end

(* Add C objects and options and "custom" info from a library descriptor *)

let lib_ccobjs = ref []
let lib_ccopts = ref []
let lib_dllibs = ref []

(* See Bytelink.add_ccobjs for explanations on how options are ordered.
   Notice that here we scan .cma files given on the command line from
   left to right, hence options must be added after. *)

let add_ccobjs l =
  if not !Clflags.no_auto_link then begin
    if l.lib_custom then Clflags.custom_runtime := true;
    lib_ccobjs := !lib_ccobjs @ l.lib_ccobjs;
    lib_ccopts := !lib_ccopts @ l.lib_ccopts;
    lib_dllibs := !lib_dllibs @ l.lib_dllibs
  end

let copy_object_file oc name =
  let file_name =
    try
      Load_path.find name
    with Not_found ->
      raise(Error(File_not_found name)) in
  let ic = open_in_bin file_name in
  try
    let buffer = really_input_string ic (String.length cmo_magic_number) in
    if buffer = cmo_magic_number then begin
      let compunit_pos = input_binary_int ic in
      seek_in ic compunit_pos;
      let compunit = (input_value ic : compilation_unit) in
      Bytelink.check_consistency file_name compunit;
      copy_compunit ic oc compunit;
      close_in ic;
      [name,compunit]
    end else
    if buffer = cma_magic_number then begin
      let toc_pos = input_binary_int ic in
      seek_in ic toc_pos;
      let toc = (input_value ic : library) in
      List.iter (Bytelink.check_consistency file_name) toc.lib_units;
      add_ccobjs toc;
      List.iter (copy_compunit ic oc) toc.lib_units;
      close_in ic;
      List.map (fun u -> name, u) toc.lib_units
    end else
      raise(Error(Not_an_object_file file_name))
  with
    End_of_file -> close_in ic; raise(Error(Not_an_object_file file_name))
  | x -> close_in ic; raise x

(* Build a thin archive.  Instead of copying the members into the archive we
   only record where to find them; the members of a thin archive given as
   input are spliced in, as they are files of their own too.  Members of a
   plain archive cannot be, so those are rejected. *)

let add_thin_ccobjs l =
  if not !Clflags.no_auto_link then begin
    if l.tlib_custom then Clflags.custom_runtime := true;
    lib_ccobjs := !lib_ccobjs @ l.tlib_ccobjs;
    lib_ccopts := !lib_ccopts @ l.tlib_ccopts;
    lib_dllibs := !lib_dllibs @ l.tlib_dllibs
  end

(* Members are accumulated in reverse order.  [force_link] tells whether the
   archive the member comes from was built with -linkall. *)
let rec scan_thin_member accu ~force_link file_name =
  let ic = open_in_bin file_name in
  try
    let buffer = really_input_string ic (String.length cmo_magic_number) in
    if buffer = cmo_magic_number then begin
      let compunit_pos = input_binary_int ic in
      seek_in ic compunit_pos;
      let compunit = (input_value ic : compilation_unit) in
      close_in ic;
      Bytelink.check_consistency file_name compunit;
      (file_name, force_link, compunit) :: accu
    end else
    if buffer = cma_thin_magic_number then begin
      let toc = (input_value ic : thin_library) in
      close_in ic;
      add_thin_ccobjs toc;
      let dir = Filename.dirname file_name in
      List.fold_left
        (fun accu u ->
           scan_thin_member accu
             ~force_link:(force_link || u.tu_force_link)
             (Misc.path_from ~dir u.tu_path))
        accu toc.tlib_units
    end else
    if buffer = cma_magic_number then
      raise(Error(Full_archive_member file_name))
    else
      raise(Error(Not_an_object_file file_name))
  with
    End_of_file -> close_in ic; raise(Error(Not_an_object_file file_name))
  | x -> close_in ic; raise x

let create_thin_archive file_list lib_name =
  let members_rev =
    List.fold_left
      (fun accu name ->
         let file_name =
           try Load_path.find name
           with Not_found -> raise(Error(File_not_found name)) in
         scan_thin_member accu ~force_link:!Clflags.link_everything file_name)
      [] file_list in
  let ldeps = Linkdeps.create ~complete:false in
  List.iter
    (fun (filename, _, compunit) ->
       Bytelink.linkdeps_unit ldeps ~filename compunit)
    members_rev;
  (match Linkdeps.check ldeps with
   | None -> ()
   | Some e -> raise (Error (Link_error e)));
  let dir = Filename.dirname lib_name in
  let toc =
    { tlib_units =
        List.rev_map
          (fun (file_name, force_link, _) ->
             { tu_path = Misc.path_relative_to ~dir file_name;
               tu_force_link = force_link })
          members_rev;
      tlib_custom = !Clflags.custom_runtime;
      tlib_ccobjs = !Clflags.ccobjs @ !lib_ccobjs;
      tlib_ccopts = !Clflags.all_ccopts @ !lib_ccopts;
      tlib_dllibs = !Clflags.dllibs @ !lib_dllibs } in
  let outchan = open_out_bin lib_name in
  Misc.try_finally
    ~always:(fun () -> close_out outchan)
    ~exceptionally:(fun () -> remove_file lib_name)
    (fun () ->
       output_string outchan cma_thin_magic_number;
       output_value outchan toc)

let create_plain_archive file_list lib_name =
  let outchan = open_out_bin lib_name in
  Misc.try_finally
    ~always:(fun () -> close_out outchan)
    ~exceptionally:(fun () -> remove_file lib_name)
    (fun () ->
       output_string outchan cma_magic_number;
       let ofs_pos_toc = pos_out outchan in
       output_binary_int outchan 0;
       let units =
         List.flatten(List.map (copy_object_file outchan) file_list) in
       let ldeps = Linkdeps.create ~complete:false in
       List.iter
         (fun (filename,u) -> Bytelink.linkdeps_unit ldeps ~filename u)
         (List.rev units);
       (match Linkdeps.check ldeps with
        | None -> ()
        | Some e -> raise (Error (Link_error e)));
       let toc =
         { lib_units = (List.map snd units);
           lib_custom = !Clflags.custom_runtime;
           lib_ccobjs = !Clflags.ccobjs @ !lib_ccobjs;
           lib_ccopts = !Clflags.all_ccopts @ !lib_ccopts;
           lib_dllibs = !Clflags.dllibs @ !lib_dllibs } in
       let pos_toc = pos_out outchan in
       Emitcode.marshal_to_channel_with_possibly_32bit_compat
         ~filename:lib_name ~kind:"bytecode library"
         outchan toc;
       seek_out outchan ofs_pos_toc;
       output_binary_int outchan pos_toc;
    )

let create_archive file_list lib_name =
  if !Clflags.thin_archive then create_thin_archive file_list lib_name
  else create_plain_archive file_list lib_name

open Format_doc
module Style = Misc.Style

let report_error_doc ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %a" Style.inline_code name
  | Not_an_object_file name ->
      fprintf ppf "The file %a is not a bytecode object file"
        Location.Doc.quoted_filename name
  | Full_archive_member name ->
      fprintf ppf
        "@[<hov>The bytecode library %a@ cannot be a member of a thin \
         library,@ because it holds a copy of its own members@ instead of \
         separate files.@ Rebuild it with %a.@]"
        Location.Doc.quoted_filename name
        Style.inline_code "-thin"
  | Link_error e ->
      Linkdeps.report_error_doc ~print_filename:Location.Doc.filename ppf e

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error_doc err)
      | _ -> None
    )

let report_error = Format_doc.compat report_error_doc

let reset () =
  lib_ccobjs := [];
  lib_ccopts := [];
  lib_dllibs := []
