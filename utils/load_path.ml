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


type path = Dir of string | File of string

let path_to_string (Dir s | File s) = s

type t = path list
(* Kept in reverse order *)

let empty = []

let of_dirs l = List.rev_map (fun dir -> Dir dir) l

let of_paths l = List.rev l

let add_dir t dir = Dir dir :: t

let add_file t file = File file :: t

let dirs t =
  List.rev (List.filter_map (function Dir dir -> Some dir | File _ -> None) t)

let paths t = List.rev t

let mem = List.mem

let concat ts = List.concat (List.rev ts)

let expand_directory s t =
  List.map (function
      | Dir dir -> Dir (Misc.expand_directory s dir)
      | File file -> File (Misc.expand_directory s file)
    ) t

let find fn t = Misc.find_in_path (dirs t) fn

let find_uncap fn t = Misc.find_in_path_uncap (dirs t) fn

let find_rel fn t = Misc.find_in_path_rel (dirs t) fn

module Cache = struct
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

  module Path = struct
    type t =
      | Dir of Dir.t
      | File of string

    let create : path -> t = function
      | Dir dir -> Dir (Dir.create dir)
      | File file -> File file

    let dir dir = Dir (Dir.create dir)

    let file file = File file

    let path : t -> path = function
      | Dir dir -> Dir (Dir.path dir)
      | File file -> File file

    let files : t -> string list = function
      | Dir dir -> Dir.files dir
      | File file -> [Filename.basename file]
  end

  let paths : Path.t list ref = ref []

  let reset () =
    files := SMap.empty;
    files_uncap := SMap.empty;
    paths := []

  let get () = List.rev !paths
  let get_paths () = List.map Path.path !paths

  let add_to_maps fn basenames files files_uncap =
    List.fold_left (fun (files, files_uncap) base ->
        let fn = fn base in
        SMap.add base fn files,
        SMap.add (String.uncapitalize_ascii base) fn files_uncap
      ) (files, files_uncap) basenames

  let add (files, files_uncap) = function
    | Path.Dir dir ->
        add_to_maps (Filename.concat dir.Dir.path)
          dir.Dir.files files files_uncap
    | Path.File file ->
        add_to_maps (Fun.const file) [Filename.basename file]
          files files_uncap

  (* Optimized version of [add] below, for use in [init] and [remove_dir]: since
     we are starting from an empty cache, we can avoid checking whether a unit
     name already exists in the cache simply by adding entries in reverse
     order. *)

  let init l =
    let new_paths = List.map Path.create l in
    let new_files, new_files_uncap =
      List.fold_left add (SMap.empty, SMap.empty) new_paths in
    files := new_files;
    files_uncap := new_files_uncap;
    paths := new_paths

  let remove_dir dir =
    let new_paths =
      List.filter
        (function Path.Dir d -> Dir.path d <> dir | _ -> true) !paths in
    if List.compare_lengths new_paths !paths <> 0 then begin
      let new_files, new_files_uncap =
        List.fold_left add (SMap.empty, SMap.empty) new_paths in
      files := new_files;
      files_uncap := new_files_uncap;
      paths := new_paths
    end

  (* General purpose version of function to add a new entry to load path: We only
     add a basename to the cache if it is not already present in the cache, in
     order to enforce left-to-right precedence. *)

  let add path =
    let new_files, new_files_uncap = add (SMap.empty, SMap.empty) path in
    let first _ fn _ = Some fn in
    files := SMap.union first !files new_files;
    files_uncap := SMap.union first !files_uncap new_files_uncap;
    paths := path :: !paths

  let add_dir dir = add (Path.dir dir)

  let add_file file = add (Path.file file)

  let add_path = function
    | Dir dir -> add_dir dir
    | File file -> add_file file

  let is_basename fn = Filename.basename fn = fn

  let find fn =
    if is_basename fn then
      SMap.find fn !files
    else
      find fn (get_paths ())

  let find_uncap fn =
    if is_basename fn then
      SMap.find (String.uncapitalize_ascii fn) !files_uncap
    else
      find_uncap fn (get_paths ())
end
