(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Main program of the ocamltest test driver *)

open Ocamltest_stdlib
open Tsl_semantics

type behavior =
  | Skip_all_tests
  | Run of Environments.t

(*
let first_token filename =
  let input_channel = open_in filename in
  let lexbuf = Lexing.from_channel input_channel in
  Location.init lexbuf filename;
  let token =
    try Tsl_lexer.token lexbuf with e -> close_in input_channel; raise e
  in close_in input_channel; token

let is_test filename =
  match first_token filename with
    | exception _ -> false
    | Tsl_parser.TSL_BEGIN_C_STYLE | TSL_BEGIN_OCAML_STYLE -> true
    | _ -> false
*)

let tsl_block_of_file test_filename =
  let input_channel = open_in test_filename in
  let lexbuf = Lexing.from_channel input_channel in
  Location.init lexbuf test_filename;
  match Tsl_parser.tsl_block Tsl_lexer.token lexbuf with
    | exception e -> close_in input_channel; raise e
    | _ as tsl_block -> close_in input_channel; tsl_block

let tsl_block_of_file_safe test_filename =
  try tsl_block_of_file test_filename with
  | Sys_error message ->
    Printf.eprintf "%s\n" message;
    exit 1
  | Parsing.Parse_error ->
    Printf.eprintf "Could not read test block in %s\n" test_filename;
    exit 1

let print_usage () =
  Printf.printf "%s\n%!" Options.usage

let color bold n s =
  if Sys.isatty stdout then
    Printf.sprintf "\027[%d;%dm%s\027[0m" bold n s
  else
    s

let red s = color 1 31 s
let green s = color 0 32 s
let yellow s = color 0 33 s
let gray s = color 2 37 s

let rec run_test log common_prefix path behavior = function
  Node (testenvspec, test, env_modifiers, subtrees) ->
  Printf.printf "%s/%s %-20s %!" common_prefix (String.concat "." path) (gray test.Tests.test_name);
  let (msg, b) = match behavior with
    | Skip_all_tests -> yellow "skipped", Skip_all_tests
    | Run env ->
      let testenv0 = interprete_environment_statements env testenvspec in
      let testenv = List.fold_left apply_modifiers testenv0 env_modifiers in
      let t = Tests.run log testenv test in
      begin match t with
      | Tests.Ok env ->
          green "passed" ^ "\n", Run env
      | Tests.Skip reason ->
          yellow "skipped (" ^ reason ^ ")\n", Skip_all_tests
      | Tests.Fail (action_name, reason) ->
          let cmdline = "./ocamltest.exe foo" in
          let msg = Printf.sprintf "failed (%s)" action_name in
          red msg ^ "\n" ^ gray cmdline ^ "\n" ^ reason, Skip_all_tests
      end
  in
  Printf.printf "%s%!" msg;
  List.iteri (run_test_i log common_prefix path b) subtrees
and run_test_i log common_prefix path behavior i test_tree =
  let new_path = string_of_int (i+1) :: path in
  run_test log common_prefix new_path behavior test_tree

let get_test_source_directory test_dirname =
  if (Filename.is_relative test_dirname) then
    Sys.with_chdir test_dirname Sys.getcwd
  else test_dirname

let get_test_build_directory_prefix test_dirname =
  let ocamltestdir_variable = "OCAMLTESTDIR" in
  let root = try Sys.getenv ocamltestdir_variable with
    | Not_found -> (Filename.concat (Sys.getcwd ()) "_ocamltest") in
  if test_dirname = "." then root
  else Filename.concat root test_dirname

let test_file test_filename =
  (* Printf.printf "# reading test file %s\n%!" test_filename; *)
  let tsl_block = tsl_block_of_file_safe test_filename in
  let (rootenv_statements, test_trees) = test_trees_of_tsl_block tsl_block in
  let test_trees = match test_trees with
    | [] ->
      let default_tests = Tests.default_tests() in
      let make_tree test = Node ([], test, [], []) in
      List.map make_tree default_tests
    | _ -> test_trees in
  let used_tests = tests_in_trees test_trees in
  let used_actions = actions_in_tests used_tests in
  let action_names =
    let f act names = StringSet.add (Actions.action_name act) names in
    Actions.ActionSet.fold f used_actions StringSet.empty in
  let test_dirname = Filename.dirname test_filename in
  let test_basename = Filename.basename test_filename in
  let test_prefix = Filename.chop_extension test_basename in
  let test_directory =
    if test_dirname="." then test_prefix
    else Filename.concat test_dirname test_prefix in
  let test_source_directory = get_test_source_directory test_dirname in
  let hookname_prefix = Filename.concat test_source_directory test_prefix in
  let test_build_directory_prefix =
    get_test_build_directory_prefix test_directory in
  Sys.make_directory test_build_directory_prefix;
  Sys.with_chdir test_build_directory_prefix
    (fun () ->
       let log =
         if !Options.log_to_stderr then stderr else begin
           let log_filename = test_prefix ^ ".log" in
           open_out log_filename
         end in
       let install_hook name =
         let hook_name = Filename.make_filename hookname_prefix name in
         if Sys.file_exists hook_name then begin
           let hook = Actions_helpers.run_hook hook_name in
           Actions.set_hook name hook
         end in
       StringSet.iter install_hook action_names;

       let reference_filename = Filename.concat
           test_source_directory (test_prefix ^ ".reference") in
       let initial_environment = Environments.from_bindings
           [
             Builtin_variables.test_file, test_basename;
             Builtin_variables.reference, reference_filename;
             Builtin_variables.test_source_directory, test_source_directory;
             Builtin_variables.test_build_directory_prefix, test_build_directory_prefix;
           ] in
       let root_environment =
         interprete_environment_statements initial_environment rootenv_statements in
       let rootenv = Environments.initialize log root_environment in
       let common_prefix = Printf.sprintf "%40s" (Filename.chop_extension test_basename) in
       List.iteri
         (run_test_i log common_prefix [] (Run rootenv))
         test_trees;
       Actions.clear_all_hooks();
       if not !Options.log_to_stderr then close_out log
    )

let rec make_relative curr path =
  if path = curr then
    Filename.current_dir_name
  else
    Filename.concat (make_relative curr (Filename.dirname path)) (Filename.basename path)

let test_files dir files =
  Printf.printf "... Entering %s\n%!" dir;
  List.iter (fun file -> test_file (make_relative (Sys.getcwd ()) (Filename.concat dir file))) files

let get_tests paths =
  let add (acc, cur_dir, cur_files) path =
    let path_dirname = Sys.with_chdir (Filename.dirname path) Sys.getcwd in
    Printf.printf "add: cur_dir=%S path=%S path_dirname=%S\n%!" cur_dir path path_dirname;
    if path_dirname = cur_dir then
      acc, cur_dir, Filename.basename path :: cur_files
    else
      ((cur_dir, cur_files) :: acc), path_dirname, [Filename.basename path]
  in
  let rec walk acc path =
    let ocamltests = Filename.concat path "ocamltests" in
    let acc =
      if Sys.file_exists ocamltests then
        let lines = with_open_in ocamltests input_lines in
        List.fold_left add acc (List.map (Filename.concat path) lines)
      else
        acc
    in
    Array.fold_left (fun acc entry ->
        let path = Filename.concat path entry in
        if Sys.is_directory path then
          walk acc path
        else
          acc
      ) acc (Sys.readdir path)
  in
  let acc =
    List.fold_left (fun acc path ->
        if Sys.is_directory path then
          walk acc path
        else
          add acc path
      ) ([], "", []) paths
  in
  let acc =
    match acc with
    | acc, _, [] ->
        acc
    | acc, cur_dir, cur_files ->
        (cur_dir, cur_files) :: acc
  in
  List.rev acc

let main () =
  if !Options.paths_to_test = [] then begin
    print_usage();
    exit 1
  end;
  let tests = get_tests !Options.paths_to_test in
  List.iter (fun (dir, tests) -> test_files dir tests) tests

let _ = main()
