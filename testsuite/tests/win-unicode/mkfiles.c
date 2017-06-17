#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#ifdef _WIN32

#include <Windows.h>
#include <io.h>

#define _tfopen _wfopen

/* Returns an OCaml string with the UTF-16 representation of [s], *including* the final (2-byte) NULL */
CAMLprim value caml_to_utf16(value s)
{
  CAMLparam1(s);
  CAMLlocal1(w);
  int size;
  size = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, String_val(s), caml_string_length(s), NULL, 0);
  if (size == 0) caml_failwith("Invalid UTF-8");
  w = caml_alloc_string((size + 1) * sizeof(wchar_t));
  ((wchar_t *)String_val(w))[size] = 0;
  size = MultiByteToWideChar(CP_UTF8, 0, String_val(s), caml_string_length(s), (wchar_t *)String_val(w), size);
  assert(size != 0);
  CAMLreturn(w);
}

#else

#include <unistd.h>
#include <sys/stat.h>

#define _tfopen fopen

CAMLprim value caml_to_utf16(value s)
{
  CAMLparam1(s);
  CAMLlocal1(w);
  w = caml_alloc_string(caml_string_length(s));
  memcpy(String_val(w), String_val(s), caml_string_length(s));
  CAMLreturn(w);
}

#endif

CAMLprim value caml_create_file(value s, value contents)
{
  CAMLparam2(s, contents);
  FILE * f;
  f = _tfopen((_TCHAR *)String_val(s), _T("w"));
  if (f == NULL) caml_failwith("fopen failed");
  fwrite(String_val(contents), 1, caml_string_length(contents), f);
  fclose(f);
  CAMLreturn(Val_unit);
}
