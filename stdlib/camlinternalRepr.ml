(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type field_desc =
  {
    fld_name: string;
    fld_type: type_desc;
  }

and variant_args =
  | Cstr_tuple of type_desc iarray
  | Cstr_record of type_desc

and constructor_desc =
  {
    cstr_name: string;
    cstr_args: variant_args;
  }

and variant_repr =
  | Variant_regular
  | Variant_unboxed

and variant_desc =
  {
    var_repr: variant_repr;
    var_const: string iarray;
    var_nonconst: constructor_desc iarray;
  }

and record_repr =
  | Record_regular
  | Record_unboxed
  | Record_float
  | Record_extension

and record_desc =
  {
    rec_repr: record_repr;
    rec_fields: field_desc iarray;
  }

and type_def =
  {
    td_name: string;
    td_params: int;
    td_desc: type_desc;
  }

and type_desc =
  | Tabstract
  | Tint
  | Tchar
  | Tstring
  | Tbytes
  | Tfloat
  | Tnativeint
  | Tint32
  | Tint64
  | Tarray of type_desc
  | Tiarray of type_desc
  | Tlist of type_desc
  | Tlazy of type_desc
  | Tfloatarray
  | Ttuple of type_desc iarray
  | Tarrow of type_desc * type_desc
  | Tvariant of variant_desc
  | Trecord of record_desc
  | Tparam of int
  | Tconstr of int * type_desc iarray
  | Tlet of type_def list * type_desc

type print_env =
  {
    params: (print_env * type_desc) iarray;
    defs: type_def option array;
  }

let pp_sep ppf c =
  Format.pp_print_char ppf c;
  Format.pp_print_space ppf ()

let pp_field fld_name pp ppf obj =
  Format.pp_open_hvbox ppf 0;
  Format.pp_print_string ppf fld_name;
  Format.pp_print_char ppf ' ';
  Format.pp_print_char ppf '=';
  Format.pp_print_break ppf 1 2;
  pp ppf obj;
  Format.pp_close_box ppf ()

let arg_or args i default =
  if 0 <= i && i < Iarray.length args then Iarray.unsafe_get args i else default

let extend_defs defs new_defs =
  let old_len = Array.length defs in
  let new_len = max old_len (List.length new_defs) in
  let defs' = Array.make new_len None in
  Array.blit defs 0 defs' 0 old_len;
  List.iteri (fun i def -> defs'.(i) <- Some def) new_defs;
  defs'

let print_left_paren ppf paren =
  if paren then begin
    Format.pp_open_hovbox ppf 1;
    Format.pp_print_char ppf '('
  end

let print_right_paren ppf paren =
  if paren then begin
    Format.pp_print_char ppf ')';
    Format.pp_close_box ppf ()
  end

let rec print_obj paren env ty ppf obj =
  match ty with
  | Tabstract ->
      Format.pp_print_string ppf "<abstr>"
  | Tint ->
      Format.fprintf ppf "%d" (Obj.obj obj)
  | Tchar ->
      Format.fprintf ppf "%C" (Obj.obj obj)
  | Tstring | Tbytes ->
      Format.fprintf ppf "%S" (Obj.obj obj)
  | Tfloat ->
      Format.pp_print_float ppf (Obj.obj obj)
  | Tnativeint ->
      Format.fprintf ppf "%ndn" (Obj.obj obj)
  | Tint32 ->
      Format.fprintf ppf "%ldl" (Obj.obj obj)
  | Tint64 ->
      Format.fprintf ppf "%LdL" (Obj.obj obj)
  | Tarray ty | Tiarray ty ->
      Format.pp_open_hovbox ppf 2;
      Format.pp_print_string ppf "[|";
      if Obj.tag obj = Obj.double_array_tag then begin
        let obj = (Obj.obj obj : floatarray) in
        for i = 0 to Float.Array.length obj - 1 do
          if i > 0 then pp_sep ppf ';';
          Format.pp_print_float ppf (Float.Array.unsafe_get obj i)
        done
      end else begin
        for i = 0 to Obj.size obj - 1 do
          if i > 0 then pp_sep ppf ';';
          print_obj false env ty ppf (Obj.field obj i)
        done
      end;
      Format.pp_print_string ppf "|]";
      Format.pp_close_box ppf ()
  | Tlist ty ->
      let rec pp_list first ppf obj =
        if Obj.is_block obj then begin
          if not first then pp_sep ppf ';';
          print_obj false env ty ppf (Obj.field obj 0);
          pp_list false ppf (Obj.field obj 1)
        end
      in
      Format.pp_open_hovbox ppf 1;
      Format.pp_print_char ppf '[';
      pp_list true ppf obj;
      Format.pp_print_char ppf ']';
      Format.pp_close_box ppf ()
  | Tlazy _ ->
      Format.pp_print_string ppf "<lazy>"
  | Tfloatarray ->
      let obj = (Obj.obj obj : floatarray) in
      Format.pp_open_hovbox ppf 2;
      Format.pp_print_string ppf "[|";
      for i = 0 to Float.Array.length obj - 1 do
        if i > 0 then pp_sep ppf ';';
        Format.pp_print_float ppf (Float.Array.unsafe_get obj i)
      done;
      Format.pp_print_string ppf "|]";
      Format.pp_close_box ppf ()
  | Ttuple tys ->
      Format.pp_open_hovbox ppf 1;
      Format.pp_print_char ppf '(';
      for i = 0 to Obj.size obj - 1 do
        if i > 0 then pp_sep ppf ',';
        print_obj false env (Iarray.unsafe_get tys i) ppf (Obj.field obj i)
      done;
      Format.pp_print_char ppf ')';
      Format.pp_close_box ppf ()
  | Tarrow _ ->
      Format.pp_print_string ppf "<fun>"
  | Tvariant v ->
      pp_variant paren env v ppf obj
  | Trecord r ->
      pp_record env r ppf obj
  | Tparam i ->
      if 0 <= i && i < Iarray.length env.params then begin
        let env', desc = Iarray.unsafe_get env.params i in
        print_obj paren env' desc ppf obj
      end else
        Format.fprintf ppf "<param %d>" i
  | Tconstr (id, args) ->
      if id < 0 || id >= Array.length env.defs then
        Format.fprintf ppf "<constr %d>" id
      else begin
        match env.defs.(id) with
        | None ->
            Format.fprintf ppf "<constr %d>" id
        | Some def ->
            let params =
              Iarray.init def.td_params (fun i -> (env, arg_or args i Tabstract))
            in
            print_obj paren {params; defs = env.defs} def.td_desc ppf obj
      end
  | Tlet (defs, ty) ->
      let defs = extend_defs env.defs defs in
      print_obj paren {env with defs} ty ppf obj

and pp_variant paren env v ppf obj =
  match v.var_repr with
  | Variant_unboxed ->
      let cstr = Iarray.unsafe_get v.var_nonconst 0 in
      print_left_paren ppf paren;
      Format.pp_print_string ppf cstr.cstr_name;
      Format.pp_print_char ppf ' ';
      let ty = match cstr.cstr_args with Cstr_tuple tyl -> Iarray.unsafe_get tyl 0 | Cstr_record ty -> ty in
      print_obj true env ty ppf obj;
      print_right_paren ppf paren
  | Variant_regular ->
      if Obj.is_int obj then
        Format.pp_print_string ppf (Iarray.unsafe_get v.var_const (Obj.obj obj))
      else begin
        let cstr = Iarray.unsafe_get v.var_nonconst (Obj.tag obj) in
        print_left_paren ppf paren;
        begin match cstr.cstr_args with
        | Cstr_tuple tys ->
            Format.pp_print_string ppf cstr.cstr_name;
            Format.pp_print_char ppf ' ';
            let unary = Iarray.length tys = 1 in
            if not unary then Format.pp_print_char ppf '(';
            for i = 0 to Iarray.length tys - 1 do
              if i > 0 then pp_sep ppf ',';
              print_obj unary env (Iarray.unsafe_get tys i) ppf (Obj.field obj i)
            done;
            if not unary then Format.pp_print_char ppf ')'
        | Cstr_record ty ->
            Format.pp_print_string ppf cstr.cstr_name;
            Format.pp_print_char ppf ' ';
            print_obj true env ty ppf obj
        end;
        print_right_paren ppf paren
      end

and pp_record env rd ppf obj =
  Format.pp_open_hovbox ppf 1;
  Format.pp_print_char ppf '{';
  begin match rd.rec_repr with
  | Record_unboxed ->
      let f = Iarray.unsafe_get rd.rec_fields 0 in
      pp_field f.fld_name (print_obj false env f.fld_type) ppf obj
  | Record_regular ->
      for i = 0 to Obj.size obj - 1 do
        let f = Iarray.unsafe_get rd.rec_fields i in
        if i > 0 then pp_sep ppf ';';
        pp_field f.fld_name (print_obj false env f.fld_type) ppf (Obj.field obj i)
      done
  | Record_extension ->
      for i = 1 to Obj.size obj - 1 do
        let f = Iarray.unsafe_get rd.rec_fields i in
        if i > 0 then pp_sep ppf ';';
        pp_field f.fld_name (print_obj false env f.fld_type) ppf (Obj.field obj i)
      done
  | Record_float ->
      let obj = (Obj.obj obj : floatarray) in
      for i = 0 to Float.Array.length obj - 1 do
        let f = Iarray.unsafe_get rd.rec_fields i in
        if i > 0 then pp_sep ppf ';';
        pp_field f.fld_name Format.pp_print_float ppf (Float.Array.unsafe_get obj i)
      done
  end;
  Format.pp_print_char ppf '}';
  Format.pp_close_box ppf ()

let print_obj ppf desc obj =
  print_obj false {params = [||]; defs = [||]} desc ppf obj
