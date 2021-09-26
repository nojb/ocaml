type t

val empty: t

val dirs: t -> string list

val of_dirs: string list -> t

val append_dir: string -> t -> t

val append_dirs: string list -> t -> t

val prepend_dir: string -> t -> t

val remove_dirs: string list -> t -> t

val concat: t list -> t

val expand_directory: string -> t -> t

val rev_map_to_list: (string -> 'a) -> t -> 'a list

val iter: (string -> unit) -> t -> unit

val find: string -> t -> string

val find_uncap: string -> t -> string
