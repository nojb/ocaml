type t = string list (* reverse order *)

let empty = []

let dirs t = List.rev t

let of_dirs t = List.rev t

let append_dir dir t = dir :: t

let append_dirs dirs t = List.rev_append dirs t

let prepend_dir dir t = t @ [dir]

let remove_dirs dirs t =
  List.filter (fun d -> not (List.mem d dirs)) t

let concat l = List.concat (List.rev l)

let expand_directory s l = List.map (Misc.expand_directory s) l

let rev_map_to_list f t = List.map f t

let iter f t = List.iter f (List.rev t)

let find fn t = Misc.find_in_path (List.rev t) fn

let find_uncap fn t = Misc.find_in_path_uncap (List.rev t) fn
