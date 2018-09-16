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
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include "unixsupport.h"
#include <time.h>
#include <errno.h>

static value alloc_tm(struct tm *tm)
{
  value res;
  res = caml_alloc_small(9, 0);
  Field(res,0) = Val_int(tm->tm_sec);
  Field(res,1) = Val_int(tm->tm_min);
  Field(res,2) = Val_int(tm->tm_hour);
  Field(res,3) = Val_int(tm->tm_mday);
  Field(res,4) = Val_int(tm->tm_mon);
  Field(res,5) = Val_int(tm->tm_year);
  Field(res,6) = Val_int(tm->tm_wday);
  Field(res,7) = Val_int(tm->tm_yday);
  Field(res,8) = tm->tm_isdst ? Val_true : Val_false;
  return res;
}

CAMLprim value unix_gmtime(value t)
{
  time_t clock;
  struct tm * tm;
  clock = (time_t) Double_val(t);
  tm = gmtime(&clock);
  if (tm == NULL) unix_error(EINVAL, "gmtime", Nothing);
  return alloc_tm(tm);
}

static int isdst(void)
{
  TIME_ZONE_INFORMATION tz;
  return (GetTimeZoneInformation(&tz) == TIME_ZONE_ID_DAYLIGHT) ? 1 : 0;
}

CAMLprim value unix_localtime(value t)
{
  struct tm tm;
  FILETIME ft;
  SYSTEMTIME utcTime, localTime;

  unix_time_to_FILETIME(Double_val(t), &ft);

  if (FileTimeToSystemTime(&ft, &utcTime) == 0) {
    win32_maperr(GetLastError());
    uerror("localtime", Nothing);
  }

  if (SystemTimeToTzSpecificLocalTime(NULL, &utcTime, &localTime) == 0) {
    win32_maperr(GetLastError());
    uerror("localtime", Nothing);
  }

  if (SYSTEMTIME_to_tm(&localTime, isdst(), &tm) == 0) {
    win32_maperr(GetLastError());
    uerror("localtime", Nothing);
  }

  return alloc_tm(&tm);
}

CAMLprim value unix_mktime(value t)
{
  CAMLparam1(t);
  CAMLlocal3(res, tmval, clkval);
  struct tm tm;
  SYSTEMTIME time, utcTime;
  FILETIME fileTime;
  __time64_t clock;

  time.wSecond = Int_val(Field(t, 0));
  time.wMinute = Int_val(Field(t, 1));
  time.wHour = Int_val(Field(t, 2));
  time.wDay = Int_val(Field(t, 3));
  time.wMonth = Int_val(Field(t, 4)) + 1;
  time.wYear = Int_val(Field(t, 5)) + 1900;
  time.wMilliseconds = 0;

  if (TzSpecificLocalTimeToSystemTime(NULL, &time, &utcTime) == 0) {
    win32_maperr(GetLastError());
    uerror("mktime", Nothing);
  }

  if (SystemTimeToFileTime(&utcTime, &fileTime) == 0) {
    win32_maperr(GetLastError());
    uerror("mktime", Nothing);
  }

  if (FILETIME_to_unix_time(&fileTime, &clock, 0) == 0) {
    win32_maperr(GetLastError());
    uerror("mktime", Nothing);
  }

  if (FileTimeToSystemTime(&fileTime, &utcTime) == 0) {
    win32_maperr(GetLastError());
    uerror("mktime", Nothing);
  }

  if (SystemTimeToTzSpecificLocalTime(NULL, &utcTime, &time) == 0) {
    win32_maperr(GetLastError());
    uerror("mktime", Nothing);
  }

  if (SYSTEMTIME_to_tm(&time, isdst(), &tm) == 0) {
    win32_maperr(GetLastError());
    uerror("mktime", Nothing);
  }

  tmval = alloc_tm(&tm);
  clkval = caml_copy_double((double) clock / 10000000.0);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = clkval;
  Field(res, 1) = tmval;

  CAMLreturn(res);
}
