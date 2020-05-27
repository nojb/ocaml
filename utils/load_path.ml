(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Rev_list = Misc.Rev_list

module Inc = struct
  type entry =
    | File of string
    | Dir of string

  type t = entry Rev_list.t
  (* Kept in reverse order *)

  let entry_to_dir = function
    | File _ -> None
    | Dir x -> Some x

  let map_entry f = function
    | File x -> File (f x)
    | Dir x -> Dir (f x)

  let empty = Rev_list.empty

  let add = Rev_list.add

  let add_file x l = add (File x) l

  let add_dir x l = add (Dir x) l

  let from_dirs l = Rev_list.of_list (List.map (fun s -> Dir s) l)

  let dirs l = Rev_list.to_list (Rev_list.filter_map entry_to_dir l)

  let from_paths = Rev_list.of_list

  let paths = Rev_list.to_list

  let mem x l = Rev_list.exists (fun y -> x = y) l (* FIXME *)

  let expand_directory dir = Rev_list.map (map_entry (Misc.expand_directory dir))

  let concat = Rev_list.concat

  let append = Rev_list.append

  let find s l = Misc.find_in_path (dirs l) s (* FIXME *)

  let find_rel s l = Misc.find_in_path_rel (dirs l) s (* FIXME *)

  let find_uncap s l = Misc.find_in_path_uncap (dirs l) s (* FIXME *)
end

module SMap = Misc.Stdlib.String.Map

(* Mapping from basenames to full filenames *)
type registry = string SMap.t ref

let files : registry = ref SMap.empty
let files_uncap : registry = ref SMap.empty

module Dir = struct
  type t = {
    path : string;
    files : string list;
  }

  let path t = t.path
  let files t = t.files

  (* For backward compatibility reason, simulate the behavior of
     [Misc.find_in_path]: silently ignore directories that don't exist
     + treat [""] as the current directory. *)
  let readdir_compat dir =
    try
      Sys.readdir (if dir = "" then Filename.current_dir_name else dir)
    with Sys_error _ ->
      [||]

  let create path =
    { path; files = Array.to_list (readdir_compat path) }
end

module Entry = struct
  type t =
    | Dir of Dir.t
    | File of string

  let path = function
    | Dir x -> Inc.Dir (Dir.path x)
    | File x -> Inc.File x
end

let entries = ref (Rev_list.empty : Entry.t Rev_list.t)

let reset () =
  files := SMap.empty;
  files_uncap := SMap.empty;
  entries := Rev_list.empty

let get () = Rev_list.to_list !entries
let get_paths () = Rev_list.map Entry.path !entries
let get_dirs () = Inc.dirs (get_paths ())

let add_entry entry =
  let add_file fn base =
    let fn = fn base in
    files := SMap.add base fn !files;
    files_uncap := SMap.add (String.uncapitalize_ascii base) fn !files_uncap;
  in
  begin match entry with
  | Entry.Dir dir ->
      List.iter (add_file (Filename.concat dir.Dir.path)) dir.Dir.files;
  | Entry.File file ->
      add_file (Fun.const file) (Filename.basename file)
  end;
  entries := Rev_list.add entry !entries

let add x =
  add_entry
    (match x with
     | Inc.File file -> Entry.File file
     | Inc.Dir dir -> Entry.Dir (Dir.create dir))

let remove_dir dir =
  let new_entries = get_paths () in
  let new_entries =
    Rev_list.filter (function Inc.Dir d -> d <> dir | _ -> true) new_entries in
  if Rev_list.compare_length new_entries !entries <> 0 then begin
    reset ();
    List.iter add (Inc.paths new_entries)
  end

let add_dir dir = add (Inc.Dir dir)
let add_file file = add (Inc.File file)

let init l =
  reset ();
  List.iter add (Inc.paths l)

let is_basename fn = Filename.basename fn = fn

let find fn =
  if is_basename fn then
    SMap.find fn !files
  else
    Inc.find fn (get_paths ())

let find_uncap fn =
  if is_basename fn then
    SMap.find (String.uncapitalize_ascii fn) !files_uncap
  else
    Inc.find_uncap fn (get_paths ())
