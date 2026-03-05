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

val print_obj : Format.formatter -> type_desc -> Obj.t -> unit
