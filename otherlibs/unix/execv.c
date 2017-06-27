/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/osdeps.h>
#include "unixsupport.h"

CAMLprim value unix_execv(value path, value args)
{
  charnat * wpath;
  charnat ** argv;
  caml_unix_check_path(path, "execv");
  argv = cstringvect(args, "execv");
  wpath = caml_stat_strdup_to_utf16(String_val(path));
  (void) _texecv(wpath, argv);
  caml_stat_free(wpath);
  cstringvect_free(argv);
  uerror("execv", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                /* from smart compilers */
}
