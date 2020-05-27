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

(** Management of include directories.

    This module offers a high level interface to locating files in the
    load path, which is constructed from [-I] command line flags and a few
    other parameters.

    It makes the assumption that the contents of include directories
    doesn't change during the execution of the compiler.
*)

module Inc : sig
  (** Include specifications.

      An {e include specification} is a sequence of directories and/or filenames
      which can be used to initialize a load path. Include specifications are
      typically created from a sequence of [-I] and/or [-inc] flags on the
      command line.

      One can modify the current load path directly after initializing, without
      going through an include specification, but at any time, it is possible to
      retrieve at a corresponding include specification by using the
      {!get_paths} function, so that the current load path is always equivalent
      to [init (get_paths ())].
  *)

  type t
  (** The type of include specifications. *)

  val empty : t
  (** The empty include specification. *)

  val add : string -> t -> t
  (** Append an entry to the include specification. *)

  val add_dir : string -> t -> t
  (** Append a directory to the include specification. *)

  val from_dirs : string list -> t
  (** [from_dirs l] is equivalent to [List.fold_left add_dir l empty]. *)

  val dirs : t -> string list
  (** The sub sequence of {e directories} in the given include specification. *)

  val from_paths : string list -> t
  (** [from_paths l] is equivalent to [List.fold add l empty]. *)

  val paths : t -> string list
  (** The sequence of entries in the given include specification. *)

  val mem : string -> t -> bool
  (** Whether an entry already appears in the include specification. *)

  val expand_directory : string -> t -> t
  (** Eventually expands a [+] at the beginning of every entry in the include
      specification. *)

  val concat : t list -> t
  (** Concatenate a list of include specifications. *)

  val append : t -> t -> t
  (** Append two include specifications. *)

  val find : string -> t -> string
  (** Search a file in a list of directories. *)

  val find_rel : string -> t -> string
  (** Search a relative file in a list of directories. *)

  val find_uncap : string -> t -> string
  (** Same, but search also for uncapitalized name, i.e. if name is Foo.ml,
      allow /path/Foo.ml and /path/foo.ml to match. *)
end

val add_dir : string -> unit
(** Add a directory to the load path *)

val remove_dir : string -> unit
(** Remove a directory from the load path *)

val reset : unit -> unit
(** Remove all directories *)

val init : Inc.t -> unit
(** [init l] is the same as [reset (); List.iter add_dir (List.rev l)] *)

val get_paths : unit -> Inc.t
(** Return the include specification corresponding to the current load path. *)

val get_dirs : unit -> string list
(** Return the sub sequence of directories in the include specification
    corresponding to the current load path. *)

val find : string -> string
(** Locate a file in the load path. Raise [Not_found] if the file
    cannot be found. This function is optimized for the case where the
    filename is a basename, i.e. doesn't contain a directory
    separator. *)

val find_uncap : string -> string
(** Same as [find], but search also for uncapitalized name, i.e.  if
    name is Foo.ml, allow /path/Foo.ml and /path/foo.ml to match. *)

module Dir : sig
  type t
  (** Represent one directory in the load path. *)

  val create : string -> t

  val path : t -> string

  val files : t -> string list
  (** All the files in that directory. This doesn't include files in
      sub-directories of this directory. *)
end

val add : Dir.t -> unit

val get : unit -> Dir.t list
(** Same as [get_paths ()], except that it returns a [Dir.t list]. *)
