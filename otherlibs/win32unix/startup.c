/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt     */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <caml/mlvalues.h>
#include "winworker.h"
#include "windbug.h"

CAMLprim value win_startup(value unit)
{
  WSADATA wsaData;

  (void) WSAStartup(MAKEWORD(2, 0), &wsaData);

  worker_init();

  return Val_unit;
}

CAMLprim value win_cleanup(value unit)
{
  worker_cleanup();

  (void) WSACleanup();

  return Val_unit;
}
