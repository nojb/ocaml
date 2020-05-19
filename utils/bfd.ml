(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Nicolas Ojeda Bar, LexiFi                       *)
(*                                                                        *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let name_at ?max buf start =
  assert (start >= 0 && start <= Bytes.length buf);
  let rec loop pos =
    if pos >= Bytes.length buf ||
       Bytes.get buf pos = '\000' ||
       (match max with None -> false | Some n -> pos - start >= n)
    then
      Bytes.sub_string buf start (pos - start)
    else
      loop (succ pos)
  in
  loop start

let array_find f a =
  let rec loop i =
    if i >= Array.length a then None
    else if f a.(i) then Some a.(i)
    else loop (succ i)
  in
  loop 0

let array_find_map f a =
  let rec loop i =
    if i >= Array.length a then None
    else begin
      match f a.(i) with
      | None -> loop (succ i)
      | Some _ as r -> r
    end
  in
  loop 0

let unsigned_of_int32 n =
  assert (n >= 0l);
  Int64.of_int32 n

type endianness =
  | LittleEndian
  | BigEndian

type bitness =
  | X86
  | X64

module type MAGIC = sig
  val endianness: endianness
  val bitness: bitness
end

module Decode (M : MAGIC) = struct
  include M

  let word_size =
    match bitness with
    | X64 -> 8
    | X86 -> 4

  let get_int16 buf idx =
    match endianness with
    | LittleEndian -> Bytes.get_uint16_le buf idx
    | BigEndian    -> Bytes.get_uint16_be buf idx

  let get_int32 buf idx =
    match endianness with
    | LittleEndian -> Bytes.get_int32_le buf idx
    | BigEndian    -> Bytes.get_int32_be buf idx

  let get_int buf idx =
    Int32.to_int (get_int32 buf idx)

  let get_int64 buf idx =
    match endianness with
    | LittleEndian -> Bytes.get_int64_le buf idx
    | BigEndian    -> Bytes.get_int64_be buf idx

  let get_word buf idx =
    match bitness with
    | X64 -> get_int64 buf idx
    | X86 -> unsigned_of_int32 (get_int32 buf idx)
end

module type READ = sig
  val seek: int64 -> unit
  val advance: int -> unit
  val read_bytes: int -> bytes
end

module type S = sig
  val defines_symbol: string -> bool
  val symbol_offset: string -> int64 option
end

(* Files are parsed by instantiating the corresponding functor. *)

module ELF (R : READ) : S = struct

  include R

  let identification =
    seek 0L;
    read_bytes 16

  module M = struct
    let bitness =
      match Bytes.get identification 4 with
      | '\x01' -> X86
      | '\x02' -> X64
      | _ as n -> Printf.ksprintf failwith "Unexpected ELF CLASS (0x%x)" (Char.code n)

    let endianness =
      match Bytes.get identification 5 with
      | '\x01' -> LittleEndian
      | '\x02' -> BigEndian
      | _ as n -> Printf.ksprintf failwith "Unexpected ELF DATA (0x%x)" (Char.code n)
  end

  include Decode (M)

  let header_size =
    40 + 3 * word_size

  type header =
    {
      e_shnum: int;
      e_shentsize: int;
      e_shoff: int64;
      e_shstrndx: int;
    }

  let {e_shnum; e_shentsize; e_shoff; e_shstrndx} =
    seek 0L;
    let buf         = read_bytes header_size in
    let e_shnum     = get_int16 buf (36 + 3 * word_size) in
    let e_shentsize = get_int16 buf (34 + 3 * word_size) in
    let e_shoff     = get_word  buf (24 + 2 * word_size) in
    let e_shstrndx  = get_int16 buf (38 + 3 * word_size) in
    {e_shnum; e_shentsize; e_shoff; e_shstrndx}

  type section =
    {
      sh_name: int;
      sh_addr: int64;
      sh_offset: int64;
      sh_size: int;
      sh_entsize: int;
      sh_name_str: string;
    }

  let read_section_body {sh_offset; sh_size; _} =
    seek sh_offset;
    read_bytes sh_size

  let sections =
    seek e_shoff;
    let mk buf =
      let sh_name    = get_int buf 0 in
      let sh_addr    = get_word buf (8 + word_size) in
      let sh_offset  = get_word buf (8 + 2 * word_size) in
      let sh_size    = Int64.to_int (get_word buf (8 + 3 * word_size)) in
      let sh_entsize = Int64.to_int (get_word buf (16 + 5 * word_size)) in
      {sh_name; sh_addr; sh_offset; sh_size; sh_entsize; sh_name_str = ""}
    in
    Array.init e_shnum (fun _ -> mk (read_bytes e_shentsize))

  let sections =
    let shstrtbl = read_section_body sections.(e_shstrndx) in
    Array.map (fun sec ->
        let sh_name_str = name_at shstrtbl sec.sh_name in
        {sec with sh_name_str}
      ) sections

  type symbol =
    {
      st_name: string;
      st_value: int64;
      st_shndx: int;
    }

  let find_section sectname =
    array_find (function {sh_name_str; _} -> sh_name_str = sectname) sections

  let symbols =
    match find_section ".dynsym" with
    | None -> [| |]
    | Some dynsym ->
        begin match find_section ".dynstr" with
        | None -> [| |]
        | Some dynstr ->
            let symstrbuf = read_section_body dynstr in
            let dynsymbuf = read_section_body dynsym in
            let mk i =
              let base     = i * dynsym.sh_entsize in
              let st_name  = name_at symstrbuf (get_int dynsymbuf base) in
              let st_value = get_word dynsymbuf (base + word_size) in
              let st_shndx =
                let off = match bitness with X64 -> 6 | X86 -> 14 in
                get_int16 dynsymbuf (base + off)
              in
              {st_name; st_value; st_shndx}
            in
            Array.init (dynsym.sh_size / dynsym.sh_entsize) mk
        end

  let symbol_offset symname =
    match array_find (fun {st_name; _} -> st_name = symname) symbols with
    | None ->
        None
    | Some {st_shndx; st_value; _} ->
        Some Int64.(add sections.(st_shndx).sh_offset (sub st_value sections.(st_shndx).sh_addr))

  let defines_symbol symname =
    Array.exists (fun {st_name; _} -> st_name = symname) symbols
end

module Mach_O (R : READ) : S = struct

  include R

  type magic =
    | MH_MAGIC
    | MH_CIGAM
    | MH_MAGIC_64
    | MH_CIGAM_64

  let magic =
    seek 0L;
    match Bytes.get_int32_ne (read_bytes 4) 0 with
    | 0xFEEDFACEl -> MH_MAGIC
    | 0xCEFAEDFEl -> MH_CIGAM
    | 0xFEEDFACFl -> MH_MAGIC_64
    | 0xCFFAEDFEl -> MH_CIGAM_64
    | n           -> Printf.ksprintf failwith "Unexpected Mach-O magic (0x%lx)" n

  module M = struct
    let bitness =
      match magic with
      | MH_MAGIC    | MH_CIGAM    -> X86
      | MH_MAGIC_64 | MH_CIGAM_64 -> X64

    let endianness =
      match magic, Sys.big_endian with
      | (MH_MAGIC | MH_MAGIC_64), false
      | (MH_CIGAM | MH_CIGAM_64), true -> LittleEndian
      | (MH_MAGIC | MH_MAGIC_64), true
      | (MH_CIGAM | MH_CIGAM_64), false -> BigEndian
  end

  include Decode (M)

  let size_int = 4

  let header_size =
    (match bitness with X64 -> 6 | X86 -> 5) * 4 + 2 * size_int

  type header =
    {
      ncmds: int;
      sizeofcmds: int;
    }

  let {ncmds; sizeofcmds = _} =
    seek 0L;
    let buf        = read_bytes header_size in
    let ncmds      = get_int buf (8 + 2 * size_int) in
    let sizeofcmds = get_int buf (12 + 2 * size_int) in
    {ncmds; sizeofcmds}

  type lc_symtab =
    {
      symoff: int32;
      nsyms: int;
      stroff: int32;
      strsize: int;
    }

  type load_command =
    | LC_SYMTAB of lc_symtab
    | OTHER

  let load_commands =
    seek (Int64.of_int header_size);
    Array.init ncmds (fun _ ->
        let buf     = read_bytes 8 in
        let cmd     = get_int32 buf 0 in
        let cmdsize = get_int   buf 4 in
        match cmd with
        | 0x2l ->
            let buf     = read_bytes 16 in
            let symoff  = get_int32 buf 0 in
            let nsyms   = get_int   buf 4 in
            let stroff  = get_int32 buf 8 in
            let strsize = get_int   buf 12 in
            LC_SYMTAB {symoff; nsyms; stroff; strsize}
        | _ ->
            advance (cmdsize - 8);
            OTHER
      )

  type symbol =
    {
      n_name: string;
      n_value: int64;
    }

  let size_nlist =
    8 + word_size

  let read_string_table off size =
    seek (unsigned_of_int32 off);
    read_bytes size

  let symbols =
    match
      array_find_map (function
          | LC_SYMTAB symtab -> Some symtab
          | _ -> None
        ) load_commands
    with
    | None -> [| |]
    | Some {symoff; nsyms; stroff; strsize} ->
        let strtbl = read_string_table stroff strsize in
        seek (unsigned_of_int32 symoff);
        let mk buf =
          let n_name  = name_at strtbl (get_int buf 0) in
          let n_value = get_word buf 8 in
          {n_name; n_value}
        in
        Array.init nsyms (fun _ -> mk (read_bytes size_nlist))

  let fix symname =
    "_" ^ symname

  let symbol_offset symname =
    let symname = fix symname in
    match array_find (function {n_name; _} -> n_name = symname) symbols with
    | None -> None
    | Some {n_value; _} -> Some n_value

  let defines_symbol symname =
    let symname = fix symname in
    Array.exists (fun {n_name; _} -> n_name = symname) symbols
end

module FlexDLL (R : READ) : S = struct

  include R

  let e_lfanew =
    seek 0x3cL;
    let buf = read_bytes 4 in
    unsigned_of_int32 (Bytes.get_int32_le buf 0)

  type machine_type =
    | IMAGE_FILE_MACHINE_AMD64
    | IMAGE_FILE_MACHINE_I386

  let header_size = 24

  let headerbuf =
    seek e_lfanew;
    read_bytes header_size

  let machine =
    seek e_lfanew;
    match Bytes.get_uint16_le headerbuf 4 with
    | 0x8664 -> IMAGE_FILE_MACHINE_AMD64
    | 0x14c  -> IMAGE_FILE_MACHINE_I386
    | n      -> Printf.ksprintf failwith "unsupported machine type (0x%x)" n

  module M = struct
    let endianness =
      LittleEndian

    let bitness =
      match machine with
      | IMAGE_FILE_MACHINE_AMD64 -> X64
      | IMAGE_FILE_MACHINE_I386  -> X86
  end

  include Decode (M)

  type header =
    {
      number_of_sections: int;
      size_of_optional_header: int;
      characteristics: int;
    }

  let {number_of_sections; size_of_optional_header; characteristics = _} =
    let number_of_sections      = get_int16 headerbuf 6 in
    let size_of_optional_header = get_int16 headerbuf 20 in
    let characteristics         = get_int16 headerbuf 22 in
    {number_of_sections; size_of_optional_header; characteristics}

  type optional_header =
    {
      base_of_code: int32;
      image_base: int64;
    }

  let {base_of_code; image_base} =
    seek (Int64.add e_lfanew (Int64.of_int header_size));
    let buf          = read_bytes size_of_optional_header in
    let base_of_code = get_int32 buf 20 in
    let image_base   = get_word  buf (match bitness with X64 -> 24 | X86 -> 28) in
    {base_of_code; image_base}

  type section =
    {
      name: string;
      virtual_size: int;
      virtual_address: int32;
      size_of_raw_data: int;
      pointer_to_raw_data: int;
    }

  let section_header_size = 40

  let sections =
    seek Int64.(add e_lfanew (of_int (header_size + size_of_optional_header)));
    let mk buf =
      let name                = name_at ~max:8 buf 0 in
      let virtual_size        = get_int   buf 8 in
      let virtual_address     = get_int32 buf 12 in
      let size_of_raw_data    = get_int   buf 16 in
      let pointer_to_raw_data = get_int   buf 20 in
      {name; virtual_size; virtual_address; size_of_raw_data; pointer_to_raw_data}
    in
    Array.init number_of_sections (fun _ -> mk (read_bytes section_header_size))

  type symbol =
    {
      name: string;
      address: int64;
    }

  let read_section_body {size_of_raw_data; pointer_to_raw_data; _} =
    seek (Int64.of_int pointer_to_raw_data);
    read_bytes size_of_raw_data

  let find_section sectname =
    array_find (function ({name; _} : section) -> name = sectname) sections

  let symbols =
    match find_section ".exptbl" with
    | None -> [| |]
    | Some ({virtual_address; _} as exptbl) ->
        let data       = read_section_body exptbl in
        let numexports = Int64.to_int (get_word data 0) in
        Array.init numexports (fun i ->
            let address = get_word data (word_size + word_size * 2 * i) in
            let nameoff = get_word data (word_size + word_size * 2 * i + word_size) in
            let name    =
              let off =
                Int64.(sub nameoff (add (unsigned_of_int32 virtual_address) image_base))
              in
              name_at data (Int64.to_int off)
            in
            {name; address}
          )

  let symbol_offset symname =
    match array_find (function {name; _} -> name = symname) symbols with
    | None -> None
    | Some {address; _} ->
        Some Int64.(sub address (add (unsigned_of_int32 base_of_code) image_base))

  let defines_symbol symname =
    Array.exists (fun {name; _} -> name = symname) symbols
end

module type T = functor (_ : READ) -> S

let guess ic =
  seek_in ic 0;
  let magic = really_input_string ic 4 in
  match magic.[0], magic.[1], magic.[2], magic.[3] with
  | '\x7F', 'E', 'L', 'F' ->
      Ok (module ELF : T)
  | '\xFE', '\xED', '\xFA', '\xCE'
  | '\xCE', '\xFA', '\xED', '\xFE'
  | '\xFE', '\xED', '\xFA', '\xCF'
  | '\xCF', '\xFA', '\xED', '\xFE' ->
      Ok (module Mach_O : T)
  | 'M', 'Z', _, _ ->
      Ok (module FlexDLL : T)
  | a, b, c, d ->
      let magic =
        Printf.sprintf "0x%02x 0x%02x 0x%02x 0x%02x"
          (Char.code a) (Char.code b) (Char.code c) (Char.code d)
      in
      Error magic

type t =
  (module S)

let with_open_in fn f =
  let ic = open_in_bin fn in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> f ic)

let read ic =
  let module R = struct
    let read_bytes len =
      let buf = Bytes.create len in
      really_input ic buf 0 len;
      buf
    let seek pos =
      LargeFile.seek_in ic pos
    let advance n =
      LargeFile.seek_in ic (Int64.add (LargeFile.pos_in ic) (Int64.of_int n))
  end in
  match guess ic with
  | Ok (module O : T) ->
      (module O (R) : S)
  | Error magic ->
      Printf.ksprintf failwith "Unsupported file format (%s)" magic

let read filename =
  with_open_in filename
    (fun ic ->
       try read ic with End_of_file ->
         Printf.ksprintf failwith "Truncated file (%s)" filename
    )

let defines_symbol (module O : S) symname =
  O.defines_symbol symname

let symbol_offset (module O : S) symname =
  O.symbol_offset symname
