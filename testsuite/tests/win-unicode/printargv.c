#include <stdio.h>
#include <locale.h>
#include <assert.h>

#ifdef _WIN32

#include <Windows.h>

int wmain(int argc, wchar_t ** argv)
{
  int len;
  char * p;

  int i;
  for (i = 0; i < argc; i ++) {
    /* printf("%S\n", argv[i]); */
    len = WideCharToMultiByte(CP_UTF8, 0, argv[i], -1, NULL, 0, NULL, NULL);
    assert(len != 0);
    p = malloc(len);
    len = WideCharToMultiByte(CP_UTF8, 0, argv[i], -1, p, len, NULL, NULL);
    assert(len != 0);
    printf("%s\n", p);
    free(p);
  }
  fflush(stdout);
  return 0;
}

#else

int main(int argc, char ** argv)
{
  int i;
  for (i = 0; i < argc; i ++) {
    printf("%s\n", argv[i]);
  }
  fflush(stdout);
  return 0;
}

#endif
